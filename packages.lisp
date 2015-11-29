;;; -*- Mode: Lisp -*-

(defpackage #:kqueue
  (:use #:cl #:cffi)
  (:export
   #:note-delete
   #:note-write
   #:note-extend
   #:note-attrib
   #:note-link
   #:note-rename
   #:note-revoke
   #:kqueue
   #:kqueue-p
   #:event
   #:make-event
   #:event-p
   #:copy-event
   #:event-path
   #:event-flags
   #:with-kqueue
   #:read-events))
