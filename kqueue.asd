;;; -*- Mode: Lisp -*-

(asdf:operate 'asdf:load-op 'cffi-grovel)

(asdf:defsystem #:kqueue
  :depends-on (cffi iolib)
  :author "Stas Boukarev"
  :license "BSD"
  :description "Interface to kqueue(2)"
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "grovel")
               (:file "kqueue")))
