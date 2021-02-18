; [[file:messages.org::*Code][Code:1]]
(unless (find-package "MSG")
  (defpackage "MESSAGE"
    (:use "COMMON-LISP")
    (:nicknames "MSG"))
    )

(in-package msg)
; Code:1 ends here

; [[file:messages.org::*Code][Code:2]]
(defmacro string-append (&rest l)
    `(concatenate 'string ,@l))

(defmacro no-messages? ()
  `(eql :silent (slot-value *message-handler* 'verbosity)))

(defmacro verbose? ()
  `(not (member (slot-value *message-handler* 'verbosity) '(:silent :normal))))

(defmacro silent? ()
  `(eq (slot-value *message-handler* 'verbosity) :silent))


(defmacro debugging? ()
  `(not (member (slot-value *message-handler* 'verbosity) '(:silent :normal :verbose))))

(defmacro verbose-debugging? ()
  `(eql (slot-value *message-handler* 'verbosity) :verbose-debugging))

(defmacro normal-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :normal))

(defmacro silence-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :silent))

(defmacro silent-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :silent))

(defmacro no-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :silent))

(defmacro verbose-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :verbose))

(defmacro debugging-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :debugging))

(defmacro all-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :verbose-debugging))

(defmacro verbose-debugging-messages ()
  `(setf (slot-value *message-handler* 'verbosity) :verbose-debugging))

(defmacro msg (&rest l)
  `(unless (no-messages?)
     (unformatted-message *message-handler* ,@l)))

(defmacro vmsg (&rest l)
  `(when (verbose?)
     (unformatted-message *message-handler* ,@l)))

(defmacro dmsg (&rest l)
  `(when (debugging?)
     (unformatted-message *message-handler* ,@l)))

(defmacro vdmsg (&rest l)
  `(when (verbose-debugging?)
     (unformatted-message *message-handler* ,@l)))

(defmacro fmsg (string &rest l)
  `(unless (silent?)
     (formatted-message *message-handler* ,string ,@l)))

(defmacro vfmsg (string &rest l)
  `(when (verbose?)
     (formatted-message *message-handler* ,string ,@l)))

(defmacro dfmsg (string &rest l)
  `(when (debugging?)
     (formatted-message *message-handler* ,string ,@l)))

(defmacro vdfmsg (string &rest l)
  `(when (verbose-debugging?)
     (formatted-message *message-handler* ,string ,@l)))

(defmacro set-destination (stream)
  `(setf (slot-value *message-handler* 'destination) ,stream))

(defmacro destination ()
  `(slot-value *message-handler 'destination))

(defmacro verbosity ()
  `(slot-value *message-handler* 'verbosity))

(defmacro fmsg-inserts-line-breaks (&optional (value t))
  `(setf (slot-value *message-handler*) ,value))

(defmacro set-indentation (num)
  `(setf (slot-value *message-handler* 'indentation) ,num))

(defmacro set-indentation-delta (num)
  `(setf (slot-value *message-handler* 'indentation-delta) ,num))
; Code:2 ends here

; [[file:messages.org::*Code][Code:3]]
(defmacro with-indentation (&rest l)
  `(progn 
     (indent) 
     (unwind-protect 
       (progn ,@l)
     (deindent))))

(defmacro with-indent (&rest l)
  `(with-indentation ,@l))

(defmacro indent ()
  `(push-indentation *message-handler*))

(defmacro deindent ()
  `(pop-indentation *message-handler*))

(defmacro with-destination (dest &rest l)
  `(progn 
     (push-destination *message-handler* ,dest)
     (unwind-protect 
       (progn ,@l)
     (pop-destination *message-handler*))))
; Code:3 ends here

; [[file:messages.org::*Code][Code:4]]
(defclass message-handler ()
  (
   (destination :initform *standard-output* :initarg :destination)
   (verbosity :initform :normal :initarg :verbosity)
   (fmsg-inserts-line-breaks :initform t :initarg :fmsg-inserts-line-breaks)
   (indentation :initform 0 :initarg :indentation)
   (indentation-delta :initform 2 :initarg :indentation-delta)
   (indentation-stack :initform nil)
   (destination-stack :initform nil)
   )
  )
; Code:4 ends here

; [[file:messages.org::*Code][Code:5]]
(defmethod push-indentation ((self message-handler))
  (with-slots (indentation indentation-stack indentation-delta) self
    (push indentation indentation-stack)
    (setq indentation (+ indentation indentation-delta))))

(defmethod pop-indentation ((self message-handler)) 
  (with-slots (indentation indentation-stack) self
    (setq indentation (or (pop indentation-stack) 0))))

(defmethod push-destination ((self message-handler) dest)
  (with-slots (destination destination-stack) self
    (push destination destination-stack)
    (setq destination dest)))

(defmethod pop-destination ((self message-handler))
  (with-slots (destination destination-stack) self
    (setq destination (or (pop destination-stack) *standard-output*))))
; Code:5 ends here

; [[file:messages.org::*Code][Code:6]]
(defmethod formatted-message ((self message-handler) format-string &rest args)
  (with-slots (destination) self
      (apply #'format
	     (cons destination 
		   (cons (prepare-string self format-string) args)))))
; Code:6 ends here

; [[file:messages.org::*Code][Code:7]]
(defmethod prepare-string ((self message-handler) string) 
  (indent-string self (add-line-break-or-not self string)))

(defmethod indent-string ((self message-handler) string)
  (string-append (indentation-string self) string))

(defmethod indentation-string ((self message-handler))
  (with-slots (indentation) self
    (if (zerop indentation)
      ""
      (make-string indentation :initial-element #\Space))))

(defmethod add-line-break-or-not ((self message-handler) string)
  (with-slots (fmsg-inserts-line-breaks) self
    (if (not fmsg-inserts-line-breaks)
      string
      (string-append string "~&"))))
; Code:7 ends here

; [[file:messages.org::*Code][Code:8]]
(defmethod unformatted-message ((self message-handler) &rest args)
  (with-slots (destination) self
    (dolist (arg (cons (indentation-string self) args))
      (if (eql 't arg)
	(fresh-line destination)
	(write arg :stream destination :escape nil)))))
; Code:8 ends here

; [[file:messages.org::*Code][Code:9]]
(export '(msg
	  dmsg
	  vmsg
	  vdmsg
	  fmsg
	  vfmsg
	  dfmsg
	  vdfmsg
	  *message-handler*
	  message-handler
	  set-destination
	  destination
	  verbosity
	  fmsg-inserts-line-breaks
	  set-indentation
	  set-indentation-delta
	  with-indentation
	  indent
	  deindent
	  with-destination
	  normal-messages
	  silence-messages
	  silent-messages
	  no-messages
	  verbose-messages
	  debugging-messages
	  verbose-debugging-messages
	  all-messages
	  ))
; Code:9 ends here

; [[file:messages.org::*Code][Code:10]]
(defparameter *message-handler* (make-instance 'message-handler))
; Code:10 ends here
