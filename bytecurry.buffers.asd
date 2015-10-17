;;; bytecurry.buffers.asd
;;;
;;; Copyright (c) 2015 Thayne McCombs

(defsystem "bytecurry.buffers"
  :description "Library to make working with buffers of data easier."
  :author "Thayne McCombs"
  :license "MIT"
  :depends-on (:uiop
               :alexandria
               :iterate
               :trivial-gray-streams
               :bordeaux-threads)
  :components ((:file "buffer")
               (:file "piped-stream" :depends-on ("buffer"))
               (:file "stream-util")
               (:file "interface" :depends-on ("buffer"
                                               "piped-stream"
                                               "stream-util"))))
