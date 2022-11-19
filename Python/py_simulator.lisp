;; [[file:python_simulator.org::*Module setup][Module setup:3]]
(unless (find-package "SIM")
  (defpackage "SIMULATOR"
    (:use "COMMON-LISP")
 ;   (:shadowing-import-from "COMMON-LISP" "NAME")
    (:nicknames "SIM"))
    )

(in-package sim)

(shadowing-import '(NAME) :cl-user)

(load "new-symbol")
(use-package 'sym)
(load "messages")
(shadowing-import 'msg:msg)
(use-package 'message)
;; Module setup:3 ends here

;; [[file:python_simulator.org::*Global variables][Global variables:2]]
(defvar *directions* '(:north :south :east :west))

(defvar *robot-command-map*
    '((:nop do-nop)
      (:forward do-move-forward)
      (:backward do-move-backward)
      (:left do-move-left)
      (:right do-move-right)
      (:turn-right do-turn-clockwise)
      (:turn-left do-turn-counterclockwise)))

(defvar *robot-percept-map*
    '((:front-sensor forward-sensor)
      (:front-bump front-bump-sensor)
      (:rear-bump rear-bump-sensor)
      (:right-bump right-bump-sensor)      
      (:left-bump left-bump-sensor)))

(export '(*robot-command-map* *robot-percept-map* *directions*))
;; Global variables:2 ends here
