#-(or bsd darwin) (error "Unsupported OS")
(include "sys/event.h" "sys/time.h" "fcntl.h")

(in-package #:kqueue)
(constant (evfilt-vnode "EVFILT_VNODE"))
(constant (note-delete "NOTE_DELETE"))
(constant (note-write "NOTE_WRITE"))
(constant (note-extend "NOTE_EXTEND"))
(constant (note-attrib "NOTE_ATTRIB"))
(constant (note-link "NOTE_LINK"))
(constant (note-rename "NOTE_RENAME"))
(constant (note-revoke "NOTE_REVOKE"))

(constant (ev-add "EV_ADD"))
(constant (ev-delete "EV_DELETE"))
(constant (ev-enable "EV_ENABLE"))
(constant (ev-disable "EV_DISABLE"))
(constant (ev-oneshot "EV_ONESHOT"))
(constant (ev-clear "EV_CLEAR"))
(constant (ev-receipt "EV_RECEIPT"))
(constant (ev-dispatch "EV_DISPATCH"))
(constant (ev-error "EV_ERROR"))

(constant (o-evtonly "O_EVTONLY"))

