;;; interface.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs

(uiop:define-package bytecurry.buffers/interface
  (:nicknames :bytecurry.buffers :buffers)
  (:use-reexport #:bytecurry.buffers/buffer
                 #:bytecurry.buffers/piped-stream
                 #:bytecurry.buffers/stream-util))
