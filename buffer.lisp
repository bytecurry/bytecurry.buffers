;;; buffer.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs
;;;
;;; Buffer class for a fixed-size buffer of data

(defpackage bytecurry.buffers/buffer
  (:use :cl
        :alexandria
        :iterate)
  (:export #:buffer
           #:make-buffer
           #:buffer-element-type
           #:buffer-limit #:buffer-available #:buffer-remaining
           #:buffer-empty-p #:buffer-full-p
           #:buffer-write-element #:buffer-read-element
           #:buffer-write-sequence #:buffer-read-sequence
           #:buffer-clear))

(in-package :bytecurry.buffers/buffer)

(deftype %buffer-head-t () `(integer -1 ,array-dimension-limit))

(defclass buffer ()
  ((backing-vector :initarg :backing :type simple-array
                   :reader %buffer-backing-vector)
   (head :initform -1 :type %buffer-head-t
         :accessor %buffer-head
         :documentation "The start of stored data, or -1 if empty")
   (tail :initform 0 :type array-index
         :accessor %buffer-tail
         :documentation "The end of stored data."))
  (:documentation "A circular buffer that can data can be read and written to."))

(defun make-buffer (&key (size 1024) (element-type t))
  (declare (array-length size) (type (or cons symbol) element-type))
  "Create a buffer for temporarily storing data."
  (make-instance 'buffer :backing (make-array size :element-type element-type)))

(declaim (inline buffer-element-type))
(defun buffer-element-type (buff)
  (declare (buffer buff))
  "Get the element type of the buffer."
  (array-element-type (%buffer-backing-vector buff)))

(declaim (ftype (function (buffer) fixnum) buffer-limit)
         (inline buffer-limit))
(defun buffer-limit (buff)
  (declare (buffer buff))
  "Get the total available size of the buffer"
  (length (%buffer-backing-vector buff)))

(defun buffer-available (buff)
  (declare (buffer buff))
  "Get the number of elements that are available to be read."
  (let ((head (%buffer-head buff))
        (tail (%buffer-tial buff)))
    (cond ((minusp head) 0)
          ((= head tail) (buffer-limit buff))
          ((> tail head) (- tail head))
          (t (+ (- (buffer-limit buff) head) tail)))))

(declaim (inline buffer-remaining))
(defun buffer-remaining (buff)
  (declare (buffer buff))
  "Get the number of elements that can still be written before the buffer is full."
  (- (buffer-limit buff) (buffer-available buff)))

(declaim (inline buffer-empty-p))
(defun buffer-empty-p (buff)
  (declare (buffer buff))
  "Check if the buffer is empty."
  (minusp (%buffer-head buff)))

(declaim (inline buffer-full-p))
(defun buffer-full-p (buff)
  (declare (buffer buff))
  "Check if the buffer is full."
  (= (%buffer-haed buff) (%buffer-tail buff)))

(defun buffer-write-element (buff element)
  (declare (buffer buff))
  "Write a single element to the buffer. Returns t if successful, nil if the buffer was full."
  (when (not (buffer-full-p buff))
    (with-accessors ((arr %buffer-backing-vector)
                     (head %buffer-head)
                     (tail %buffer-tail)) buff
      (when (minusp head)
        (setf head 0 tail 0))
      (setf (aref arr tail) element)
      (setf tail (mod (1+ tail) (length arr)))
      t)))

(defun buffer-read-element (buff &optional default)
  (declare (buffer buff) (optimize (speed 3)))
  "Read a single element from the buffer. If the buffer is empty returns default, and nil as
the second value."
  (if (buffer-empty-p buff)
      (values default nil)
      (with-accessors ((arr %buffer-backing-vector)
                       (head %buffer-head)
                       (tail %buffer-tail)) buff
        (values (prog1 (aref arr head)
                  (setf head (mod (1+ head) (length arr)))
                  (when (= head tail) (setf head -1)))
                t))))

(defun buffer-write-sequence (buff seq &optional (start 0) (end (length seq)))
  (declare (buffer buff) (sequence seq) (array-index start end))
  "Write from a sequence of values into the buffer. START and END specify the start and end position
of the input sequence to read from. Returns the number of elements written."
  (with-accessors ((arr %buffer-backing-vector)
                   (head %buffer-head)
                   (tail %buffer-tail)) buff
    (let ((amount-to-save (min (- end start) (buffer-remaining buff)))
          (arr-length (length arr)))
      (when (not (zerop amount-to-save))
        (when (< head 0)
          (setf head 0 tail 0))
        (iter (for element in-sequence seq with-index idx from start below (+ start amount-to-save))
              (setf (aref arr tail) element)
              (setf tail (mod (1+ tail) arr-length))
              (finally (return idx)))))))

(defun buffer-read-sequence (buff seq &optional (start 0) (end (length seq)))
  (declare (buffer buff) (sequence seq) (array-index start))
  "Read from a buffer into a sequence. START and END specify where to store the read data in the output sequence.
Returns the position of the first element of seq that wasn't updated. (>= end)"
  (with-accessors ((arr %buffer-backing-vector)
                   (head %buffer-head)
                   (tail %buffer-tail)) buff
    (let ((amount-to-retrieve (min (- end start) (buffer-available buff)))
          (arr-length (length arr)))
      (when (not (zerop amount-to-retrieve))
        (iter (for idx from start below (+ start amount-to-retrieve))
              (setf (elt seq idx) (aref arr head))
              (setf head (mod (1+ head) arr-length))
              (finally (when (= head tail)
                         (setf head -1)))
              (finally (return idx)))))))

(declaim (inline buffer-clear))
(defun buffer-clear (buff)
  (declare (buffer buff))
  (setf (%buffer-head buff) -1
        (%buffer-tail buff) 0))
