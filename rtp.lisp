; [[file:rtp.org::*Header, variables][Header, variables:1]]
(in-package cl-user)

(load "unify")
(load "messages")
(shadowing-import 'msg:msg)
(use-package 'msg)
; Header, variables:1 ends here

; [[file:rtp.org::*Header, variables][Header, variables:2]]
(defvar *computable-predicates*
    '((gt . >)))
; Header, variables:2 ends here

; [[file:rtp.org::*Header, variables][Header, variables:3]]
(defvar *axioms* 
    '(;; human(Marcus)
      ((human Marcus))
      ;; Pompeian(Marcus)
      ((Pompeian Marcus))
      ;; born(Marcus,40)
      ((born Marcus 40))
      ;; forall x human(x) => mortal(x)
      ((not (human ?x1)) (mortal ?x1))
      ;; forall x Pompeian(x) => died(x,79)
      ((not (Pompeian ?x2)) (died ?x2 79))
      ;; erupted(volcano,79)
      ((erupted volcano 79))
      ;; forall x, t1, t2 mortal(x) & born(x,t1) & gt(t2-t1,150) => dead(x,t2)
      ((not (mortal ?x3)) (not (born ?x3 ?t1)) (not (gt (- ?t2 ?t1) 150)) (dead ?x3 ?t2))
      ;; now = 2021
      ((= now 2021))
      ;; forall x, t [alive(x,t) => ~dead(x,t)] & [~dead(x,t) => alive(x,t)]
      ((not (alive ?x4 ?t3)) (dead ?x4 ?t3))
      ((dead ?x5 ?t4) (alive ?x5 ?t4))
      ;; forall x,t1,t2 died(x,t1) & gt(t2,t1) => dead(x,t2)
      ((not (died ?x6 ?t5)) (not (gt ?t6 ?t5)) (dead ?x6 ?t6))
      ))
; Header, variables:3 ends here

; [[file:rtp.org::*Header, variables][Header, variables:4]]
#+:sbcl (declaim (sb-ext:muffle-conditions style-warning))
; Header, variables:4 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:1]]
(defun rtp (theorem axioms &key bindings (limit 30))
; RTP and helper:1 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:2]]
(setq axioms (sort (copy-list axioms) #'(lambda (a b) 
					    (< (length a) (length b)))))
; RTP and helper:2 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:3]]
  (multiple-value-bind  (contradiction blist) 
      (rtp-recur (negate theorem) axioms :bindings bindings :limit limit)
    (if contradiction
	(fmsg "Cannot prove the theorem.")
	(fmsg "Theorem can be proven.  Bindings = ~s." blist))
    (values (not contradiction) blist)))
; RTP and helper:3 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:4]]
(defun rtp-recur (clause axioms &key bindings (limit 30))
  (with-indentation 
      (vfmsg "Attempting to find contradiction with clause ~s." clause)
    (dfmsg "[current limit=~s]" limit)
    (vdfmsg "[bindings=~s]" bindings)

    (cond
     ((null clause)
      (values  nil bindings))
     ((= limit 0)
      (fmsg "Reached futility cut-off; assuming theorem cannot be proved.")
      (values :fail bindings))
     (t
; RTP and helper:4 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:5]]
(loop for literal in clause 
	  when (and (computable-predicate? literal)
		    (not (true-predicate? literal bindings)))
	  do (multiple-value-bind (success blist) 
		 (rtp-recur (instantiate (remove literal clause :test #'equal)
					 bindings)
			    axioms
			    :bindings bindings
			    :limit (1- limit))
	       (if (not success)
		 (return-from rtp-recur (values nil blist))))
	  else do 
	       (loop for axiom in axioms do
		     (loop for aliteral in axiom do
			   (multiple-value-bind (success blist)
			       (resolves? literal aliteral bindings)
			     (dfmsg "[~s and ~s~a resolve]"
				    literal aliteral (if success "" "do not"))
			     (when success
			       (vfmsg "Resolved: ~s" clause)
			       (vfmsg "    with: ~s" axiom)
			       (multiple-value-setq (success blist)
				 (rtp-recur
				  (instantiate (resolvent literal clause aliteral axiom  blist)
					       blist)
				  axioms :bindings blist :limit (1- limit)))
			       (when (null success)
				 (return-from rtp-recur (values nil blist))))))))
      (dfmsg "[finished with loops; failure]")
      (values :fail bindings)))))
; RTP and helper:5 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:6]]
(defun computable-predicate? (literal)
  (if (eql 'not (car literal))
    (computable-predicate? (negate-literal literal))
    (get-predicate-function (car literal))))
  
(defun true-predicate? (literal &optional bindings)
  (let (value)
    (setq literal (instantiate literal bindings))
    (setq value
      (cond
       ((unbound-var-in-literal? literal)
	t)
       ((eql 'not (car literal))
	(not (true-predicate? (cadr literal) bindings)))
       (t 
	(eval `(,(get-predicate-function (car literal)) ,@(cdr literal))))))
    (dfmsg "[computable predicate ~s is ~a]"
	   literal (if  value "true" "false"))
    value))

(defun get-predicate-function  (pred)
 (cdr (assoc pred *computable-predicates*)))
; RTP and helper:6 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:7]]
(defun unbound-var-in-literal? (lit)
  (cond
   ((null lit) nil)
   ((listp lit)
    (or (unbound-var-in-literal? (car lit))
	(unbound-var-in-literal? (cdr lit))))
   ((variable? lit) t)
   (t nil)))
; RTP and helper:7 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:8]]
(defun resolves? (lita litb &optional bindings)
  (unify (negate-literal lita) litb bindings))

(defun resolvent (clause-lit clause axiom-lit axiom &optional bindings)
  (append (remove clause-lit clause :test #'equal)
	  (remove axiom-lit axiom  :test #'equal)))
; RTP and helper:8 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:9]]
;;
;; Note: this only handles one level of nesting for ANDed or ORed clauses!
;;

(defun negate (clause)
  (cond
   ((null clause) nil)
   ((> (length clause) 1)
    ;; then it's equiv to (or a b c), so return two values, ((not a)) and a
    ;; list of the negated forms of the others, as per de Morgan's law:
    (values (list (negate-literal (car clause)))
	    (mapcar #'(lambda (a) (negate (list a))) (cdr clause))))
   ((eql 'and (caar clause))
    ;; then it's (and a b c), so return (~a ~b ~c) as per de Morgan's:
    (mapcar #'negate-literal (cdr (car clause))))
   (t (list (negate-literal (car clause))))))

(defun negate-literal (lit)
  (if (eql 'not (car lit))
    (cadr lit)
    (list 'not lit)))
; RTP and helper:9 ends here

; [[file:rtp.org::*RTP and helper][RTP and helper:10]]
#+:sbcl (declaim (sb-ext:unmuffle-conditions style-warning))
; RTP and helper:10 ends here
