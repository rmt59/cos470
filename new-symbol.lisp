; [[file:new-symbol.org::*Code][Code:1]]
(unless (find-package "sym")
  (defpackage "NEWSYMBOL"
    (:use "COMMON-LISP")
    (:nicknames "SYM")
    (:export "NEW-SYMBOL")
    )) 

(in-package sym)
; Code:1 ends here

; [[file:new-symbol.org::*Code][Code:2]]
(defvar *default-symbol-prefix* 's)
; Code:2 ends here

; [[file:new-symbol.org::*Code][Code:3]]
(defclass symbol-generator ()
  (
   (symbol-prefixes :initform (make-hash-table))
   ))
; Code:3 ends here

; [[file:new-symbol.org::*Code][Code:4]]
(defmethod initialize-instance :after ((self symbol-generator) &rest l)
  (declare (ignore l))
  (setf (gethash *default-symbol-prefix* (slot-value self 'symbol-prefixes)) 0))

(defmethod make-new-symbol ((self symbol-generator) &optional
						    (symbol-prefix *default-symbol-prefix*))
  (with-slots (symbol-prefixes) self
    (when (stringp symbol-prefix)
      (setq symbol-prefix (intern symbol-prefix))) ;convert to symbol
    
    (unless symbol-prefix
      (setq symbol-prefix *default-symbol-prefix*))

    (let* ((next-count (or (gethash symbol-prefix symbol-prefixes) 0))
	   (newsym (intern (concatenate 'string (symbol-name symbol-prefix)
					(princ-to-string next-count)))))
      (setf (gethash symbol-prefix symbol-prefixes) (1+ next-count))
      newsym)))
; Code:4 ends here

; [[file:new-symbol.org::*Code][Code:5]]
(defvar *symbol-generator* (make-instance 'symbol-generator))
; Code:5 ends here

; [[file:new-symbol.org::*Code][Code:6]]
(defmacro new-symbol (&optional prefix)
  `(make-new-symbol *symbol-generator* ,prefix))
; Code:6 ends here
