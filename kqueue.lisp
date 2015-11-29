(in-package #:kqueue)

(defconstant +event-list-number+ 16)

(isys:defsyscall kqueue :int)

(defcstruct kevent
  (ident :long)
  (filter :int16)
  (flags :uint16)
  (fflags :uint32)
  (data :pointer)
  (udata :long))

(isys:defsyscall kevent :int
  (kq :int)
  (changelist :pointer)
  (nchanges :int)
  (eventlist :pointer)
  (nevents :int)
  (timeout :pointer))

;;;

(defun set-kevent (kevent fd kfilter kflags kfflags kudata)
  (with-foreign-slots ((ident filter flags fflags udata) kevent (:struct kevent))
    (setf ident fd
          filter kfilter
          flags kflags
          fflags kfflags
          udata kudata))
  kevent)

(defstruct kqueue
  kq
  kevents
  paths)

(defstruct event
  path
  flags)

(defvar *flags*
  (loop for name in
        '(note-attrib note-delete note-extend note-link note-rename note-revoke
          note-write)
        collect (cons (symbol-value name) name)))

(defun flag-to-names (flag)
  (loop for (value . name) in *flags*
        when (logtest value flag)
        collect name))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~s flags: ~{~a~^ | ~}"
            (event-path event)
            (flag-to-names (event-flags event)))))

(defun set-timespec (timespec seconds)
  (with-foreign-slots ((isys:sec isys:nsec) timespec isys:timespec)
    (multiple-value-bind (sec usec) (iolib/base:decode-timeout seconds)
      (setf isys:sec sec
            isys:nsec (* usec 1000))))
  timespec)

(defun read-events (kqueue &key timeout)
  (let* ((paths (kqueue-paths kqueue))
         (events (kqueue-kevents kqueue))
         (length
           (if timeout
               (with-foreign-object (timespec 'isys:timespec)
                 (set-timespec timespec timeout)
                 (isys:repeat-upon-eintr
                   (kevent (kqueue-kq kqueue) (null-pointer) 0 events (length paths)
                           timespec)))
               (isys:repeat-upon-eintr
                 (kevent (kqueue-kq kqueue) (null-pointer) 0 events (length paths)
                         (null-pointer))))))
    (when (plusp length)
      (loop for i below length
            for event = (mem-aptr events '(:struct kevent) i)
            for index = (foreign-slot-value event '(:struct kevent)
                                            'udata)
            for flags = (foreign-slot-value event '(:struct kevent)
                                            'fflags)
            collect (make-event :path (aref paths index)
                                :flags flags)))))

(defun call-with-kqueue (paths-with-flags function)
  (with-foreign-objects ((result '(:struct kevent) +event-list-number+)
                         (events '(:struct kevent) (length paths-with-flags)))
    (let ((kq (isys:repeat-upon-eintr
                (kqueue)))
          fds)
      (unwind-protect
           (let ((paths (make-array (length paths-with-flags))))
             (loop for (path flags) in paths-with-flags
                   for i from 0
                   for fd = (isys:open (namestring path) o-evtonly)
                   do
                   (push fd fds)
                   (setf (aref paths i) (pathname path))
                   (set-kevent (mem-aptr events '(:struct kevent) i) fd
                               evfilt-vnode (logior ev-add ev-clear) flags i))
             (isys:repeat-upon-eintr
               (kevent kq events (length paths) (null-pointer) 0 (null-pointer)))
             (funcall function (make-kqueue :kq kq
                                            :kevents result
                                            :paths paths)))
        (isys:close kq)
        (mapc #'isys:close fds)))))

(defmacro with-kqueue ((var paths-with-flags) &body body)
  `(call-with-kqueue ,paths-with-flags
                     (lambda (,var)
                       ,@body)))
