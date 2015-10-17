;;; piped-stream.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs
;;;
;;; Piped streams

(defpackage bytecurry.buffers/piped-stream
  (:use :cl
        :bytecurry.buffers/buffer
        :trivial-gray-streams
        :bordeaux-threads
        :iterate)
  (:import-from :alexandria
                #:once-only)
  (:export #:piped-stream
           #:make-piped-stream
           #:piped-stream-buffer))

(in-package #:bytecurry.buffers/piped-stream)

(defclass piped-stream (fundamental-binary-stream fundamental-input-stream fundamental-output-stream)
  ((buffer :initarg :buffer :reader piped-stream-buffer
           :type buffer
           :documentation "The buffer that backs the stream")
   (lock :initform (make-lock)
         :reader %pstream-lock)
   (read-condition :initform (make-condition-variable)
                   :reader %pstream-read-condition)
   (write-condition :initform (make-condition-variable)
                    :reader %pstream-write-condition))
  (:documentation "A bidirection stream that pipes data from its input to its
output. To avoid deadlocks it should be read from a differnt thread than it is written to.
It _is_ threadsafe, so you don't need to manage the lock yourself."))

(defun make-piped-stream (&key (size 1024))
  "Create a piped-stream.
SIZE specifies the size of the underlying buffer, and defaults to 1024."
  (make-instance 'piped-stream :buffer (make-buffer :size size :element-type '(unsigned-byte 8))))

(defmethod stream-element-type ((stream piped-stream))
  "Get the underlying element type of the stream"
  '(unsigned-byte 8))

(defmacro with-piped-stream-slots (stream &body body)
  "Execute BODY with buffer, lock, read-condition, and write-condition
bound to their respective slots on the PIPED-STREAM stream.

Note that they are bound with LET, since all of them are read-only."
  (once-only (stream)
    `(let* ((buffer (piped-stream-buffer ,stream))
            (read-condition (%pstream-read-condition ,stream))
            (write-condition (%pstream-write-condition ,stream)))
       ,@body)))

(defmethod stream-read-byte ((stream piped-stream))
  (with-piped-stream-slots stream
    (with-lock-held (lock)
      (iter (while (buffer-empty-p buffer))
            (unless (open-stream-p stream)
              (return-from stream-read-byte :eof))
            (condition-wait read-condition lock))
      (prog1 (buffer-read-element buffer)
        ;; notify writers that there is space
        (condition-notify write-condition)))))

(defmethod stream-write-byte ((stream piped-stream) byte)
  (%pipe-check-open stream)
  (with-piped-stream-slots stream
    (with-lock-held (lock)
      (iter (while (buffer-full-p buffer))
            (condition-wait write-condition lock))
      (buffer-write-element buffer byte)
      ;; notify readers that there is data available
      (condition-notify read-condition))))

(defmethod stream-read-sequence ((stream piped-stream) seq start end &key &allow-other-keys)
  (with-piped-stream-slots stream
    (with-lock-held (lock)
      (iter (initially (setf pos start))
            (for pos next (buffer-read-sequence buffer seq pos end))
            (for prev previous pos initially start)
            (when (/= prev pos)
              ;; notify writers of space available
              (condition-notify write-condition))
            (while (and (open-stream-p stream) (< pos end)))
            ;; wait for content to read
            (condition-wait read-condition lock)
            (finally (return pos))))))

(defmethod stream-write-sequence ((stream piped-stream) seq start end &key &allow-other-keys)
  (%pipe-check-open stream)
  (with-piped-stream-slots stream
    (with-lock-held (lock)
      (iter (initially (setf pos start))
            (for pos next (buffer-write-sequence buffer seq pos end))
            (for prev previous pos initially start)
            (when (/= prev pos)
              (condition-notify read-condition)) ; notify readers of content available
            (while (< pos end))
            (condition-wait write-condition lock)))) ; wait for space to write
  seq)

(defmethod stream-clear-input ((stream piped-stream))
  "Clear the buffer. clear-output does the same thing"
  (buffer-clear (piped-stream-buffer stream)))

(defmethod stream-clear-output ((stream piped-stream))
  "Clear the buffer. clear-input does the same thing"
  (buffer-clear (piped-stream-buffer stream)))

(defun %pipe-check-open (stream)
  (declare (piped-stream stream))
  "Raise an end-of-file condition."
  (unless (open-stream-p stream)
    (cerror "Write data anyway."
            (make-condition 'end-of-file :stream stream))))
