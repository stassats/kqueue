;;; -*- Mode: Lisp -*-

(defpackage #:kqueue
  (:use #:cl #:cffi)
  (:export
   #:with-kqueue
   #:read-events
   #:watch
   #:make-watch
   #:watch-p
   #:copy-watch
   #:watch-path
   #:watch-flags
   #:note-delete
   #:note-write
   #:note-extend
   #:note-attrib
   #:note-link
   #:note-rename
   #:note-revoke))
