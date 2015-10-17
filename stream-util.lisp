;;; stream-util.lisp
;;;
;;; Copyright (c) Thayne McCombs

(defpackage bytecurry.buffers/stream-util
  (:use :cl
        :iterate
        :trivial-gray-streams)
  (:import-from :alexandria
                #:array-length)
  (:export #:copy-stream
           #:make-buffered-stream))

(in-package #:bytecurry.buffers/stream-util)

(defun copy-stream (src dest &key (buffer-size 1024))
  "Copy data from a source stream to a destination stream.
Returns the total number of bytes transferred, and closes both
streams on successful completion."
  (declare (stream src dest) (array-length buffer-size))
  (let ((buffer (make-array buffer-size :element-type (stream-element-type src))))
    (iter (for num-read = (read-sequence buffer src))
          (while (plusp num-read))
          (write-sequence buffer dest :end num-read)
          (summing num-read)
          (finally (close src)
                   (close dest)))))
