;;; -*- Mode: Lisp; Package: USER -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; COS 470: UNIFY.LISP
;;;;;  
;;;;; This file contains functions to perform unification on predicate
;;;;; calculus expressions.   They are modifications of the unify functions
;;;;; in Winston and Horn.
;;;;;
;;;;; Unify has two required arguments--expressions--and one optional 
;;;;;   argument--a unifier (or binding list).  It returns two values
;;;;;   (using "values").  The first is either T or nil, depending on 
;;;;;   whether or not the unification was successful.  The second is 
;;;;;   a unifier (or updated unifier, if the optional argument was given)
;;;;;   resulting from the unification (if the unification was successful;
;;;;;   else the value of the optional argument is returned).
;;;;;
;;;;; A unifier (binding list) has the following form:
;;;;;    ((var1 val1) (var2 val2) ... (var_n val_n))
;;;;;
;;;;; The functions in this file work on two versions of Common Lisp that 
;;;;;   I have access to, Allegro Common Lisp (ACL) and Steel Bank Common Lisp
;;;;;   (SBCL).  If you use a different Lisp, you may have to make changes!
;;;;;
;;;;; Below are some suggestions on how to represent predicate calculus
;;;;; expressions in Lisp.
;;;;;
;;;;; Predicate syntax:
;;;;;    Human(Socrates) becomes (human socrates)
;;;;;    Human(x) becomes (human ?x)  -- i.e., ?x means "x" is a variable
;;;;;    Human(x) and Roman(x) becomes (and (human ?x) (roman ?x))
;;;;;    Pompeian(x) or Roman(x) becomes (or (pompeian ?x) (roman ?x))
;;;;;    ~Pompeian(x) becomes (not (pompeian ?x))
;;;;;    Bird(x) ==> Flies(x) becomes (implies (bird ?x) (flies ?x))
;;;;;    
;;;;; Quantifier syntax:
;;;;;    existential:  (exists (?x) (on ?x ?y))
;;;;;    universal:    (forall (?x) (implies (man ?x) (mortal ?x)))
;;;;;
;;;;; Variables
;;;;; ---------
;;;;;    Note that there are some functions in this file to help you deal with 
;;;;;    "AI variables", i.e., those whose name is prefaced by "?":
;;;;;       o (variable? <foo>) returns t if <foo> is a variable
;;;;;       o (varname <foo>) returns the name of the variable -- e.g., ?X ->
;;;;;         X
;;;;;       o (make-var <foo>) will create a variable whose name is <foo>, for 
;;;;;         example X -> ?X
;;;;;       o see below for info about newSymbol 
;;;;;       o find-binding, extract-value -- helps you find a variable's
;;;;;         binding in a binding list
;;;;;       o add-binding -- adds a binding for a variable to a binding list
;;;;;
;;;;;
;;;;; Author: Roy Turner
;;;;; Date: Created: 1/25/90 (Older'n you, ain't it? :-))
;;;;; Modifications:
;;;;;   o 3/14/90: prepared for class
;;;;;   o 13:04 - Thu Nov 5, 1998 -rmt- Modifications added for COS 470, F98:
;;;;;      o newSymbol symbol generator added.  Call is:
;;;;;             (newsymbol <foo>)
;;;;;        where <foo> is a symbol or a string.  A new, unique symbol based
;;;;;        on that is returned.  Any trailing numerals are stripped from the 
;;;;;        symbol, and then a new number is appended to make a unique name.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; This file is marked up for use with LP/Lisp, the MaineSAIL literate
;;; programming for Lisp tool.  (See MaineSAIL.umcs.maine.edu.)  That's what
;;; this mess is for:
;;;
;;;<explicit/>
;;;<complete/>
;;;<title>An Implementation of Unify</title>
;;;<author>Roy M. Turner\\Department of Computer Science\\University of Maine</author>
;;;<date>Fall, 2010</date>
;;;<packages>alltt,cdps-2e</packages>
;;;
;;;<insert name="intro">
;;;<insert name="symbol-generator">
;;;<doc>[[* Code *]]</doc>
;;;
;;; I haven't had a chance to change it to the Org Mode literate programming style.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+:sbcl (declaim (sb-ext:muffle-conditions warning))


;;;[
;;; 
;;; Some package-related bookkeeping.  First tell Lisp that we're in the
;;; ``User'' package, then define[[*intern-package*]], which is used by
;;; [[newSymbol]] to determine where to put newly-created symbols that you
;;; have asked it to create.  By default, it's the ``User'' package.
;;;
;;;]

(defvar *intern-package* 'user
  "Package in which to intern symbols created by unify, etc., functions.")

;;;[
;;;
;;; First, set up ``?'' to be a  macro character to allow ?X, etc., to be treated like a variable.
;;; (Don't worry about the *var* stuff for now.)
;;;
;;;]



(unless (fboundp 'string-append)
  (defmacro string-append (&rest strings)
    `(concatenate 'string ,@strings)))

(set-macro-character #\?
   #'(lambda (stream char)
	(let ((next-char (peek-char nil stream))
	      next foo)
	  (cond
	   ((equal next-char #\))
	    ;;it's a paren, so it's invalid as a variable...just
	    ;; return symbol ?
	    (setq foo (intern "?" *intern-package*))
	    foo)
	   ((equal next-char #\space)
	    (setq foo (intern "?" *intern-package*))
	    foo)
	   (t
	    (setq next (read stream t nil t))
	    (cond
	     ((atom next)
	      ;;return ?atom
	      (multiple-value-bind (thing dummy)
				   (intern  (string-append (string #\?)
							   (symbol-name next)))
				   thing))
	     (t
	      `(*var* ,next)))))))
   t)

;;;[
;;;<function (unify p1 p2 \{bindings\})>
;;;
;;; This is the primary unify function.  [[bindings]] is optional.
;;;
;;;]

(defun unify (p1 p2 &optional bindings)
  (cond
   ((variable? p1)
    (unify-variable p1 p2 bindings))
   ((variable? p2)
    (unify-variable p2 p1 bindings))
   ((and (atom p1) (atom p2)) 
    (unify-atoms p1 p2 bindings))
   ((and (listp p1) (listp p2))
    (unify-elements p1 p2 bindings))
   (t (values nil bindings))))

;;;[
;;;
;;; <function (unify-atoms p1 p2 bindings)>
;;;
;;; Two non-variable atoms unify iff they are equal.  This is basically a
;;; function included for clarity of the main function; it should probably be
;;; re-defined sometime as a macro.
;;;
;;;]

(defun unify-atoms (p1 p2 bindings)
  (values (eql p1 p2) bindings))

;;;[
;;;
;;; <function (unify-elements p1 p2 bindings)>
;;;
;;; This looks through the elements of two lists, making sure that 
;;; corresponding elements unify and maintaining appropriate bindings.
;;;
;;;]

(defun unify-elements (p1 p2 bindings)
  (let (blist matched?)
    (multiple-value-setq (matched? blist)
      (unify (first p1) (first p2) bindings))
    (cond
     ((null matched?)
      (values nil bindings))
     ((multiple-value-setq (matched? blist)
	(unify (rest p1) (rest p2) blist))
      (values matched? blist))
     (t
      (values nil bindings)))))

;;;[
;;;
;;; <function (unify-variable p1 p2 bindings)>
;;;
;;; This unifies a variable (P1) with an arbitrary expression (P2), updating 
;;; bindings as necessary
;;;
;;;]

(defun unify-variable (p1 p2 bindings)
  (cond
   ((eql p1 p2)
    (values t bindings))
   (t
    (let ((binding (find-binding p1 bindings)))
      (if binding
	  (unify (extract-value binding) p2 bindings)
	(if (inside? p1 p2 bindings)
	    (values nil bindings)
	    (values t (add-binding p1 p2 bindings))))))))

;;;[
;;;
;;; <function (find-binding var bindings \&optional not-one-of)>
;;;
;;; This looks up a variable's binding in "bindings".  The binding can have
;;; the variable in the car or the cadr.  However, since this is used
;;; elsewhere (i.e., instantiate-variable), we have to handle the case where
;;; we found the variable as the binding of another variable -- in this case,
;;; we don't want to just return the first variable!  So you can specify a
;;; list of variables in [[not-one-of]] that [[var]] won't be allowed to bind
;;; to. 
;;; 
;;; A second value is returned (via [[values]]) that indicates whether or not
;;; a binding was found.  This allows you to distinguish this from the case in
;;; which the variable was bound, but to nil.
;;;
;;;]

(defun find-binding (var bindings &optional not-one-of)
  (let ((binding
	 (car (member var bindings :test #'(lambda (a b)
					     (let ((poss (cond
							  ((eql a (car b))
							   (cadr b))
							  ((eql a (cadr b))
							   (car b)))))
					       (when (and poss
							  (not (member poss not-one-of)))
						 t)))))))
    (cond
     ((null binding) (values nil nil))
     ((eql var (car binding))
      (values binding t))
     (t (list var
	      (values (car binding) t))))))

						    



;;;[
;;;
;;; <function (extract-value binding)>
;;;
;;; This just returns the value portion of a binding.
;;;
;;;]

(defun extract-value (binding)
  (cadr binding))


;;;[
;;; 
;;; <function (inside? var expr bindings)>
;;; <function (inside-or-equal? var expr bindings)>
;;; 
;;; These together return T if [[var]] occurs in expression [[expr]].
;;; Probably [[inside-or-equal?]] should be eliminated and its code
;;; incoprorated into [[inside?]] via an flet.
;;;
;;;]

(defun inside? (var expr bindings)
  (if (equal var expr)
      nil
      (inside-or-equal? var expr bindings)))

(defun inside-or-equal? (var expr bindings)
  (cond
   ((equal var expr) t)
   ((and (not (variable? expr)) (atom expr)) nil)
   ((variable? expr)
    (let ((binding (find-binding expr bindings)))
      (when binding
	(inside-or-equal? var (extract-value binding) bindings))))
   (t (or (inside-or-equal? var (first expr) bindings)
	  (inside-or-equal? var (rest expr) bindings)))))

;;;[
;;;
;;; <function (add-binding var val bindings)>
;;;
;;; This adds a new binding of the form [[(var val)]] to [[bindings]], or creates
;;; a new binding list if [[bindings]] is nil.
;;;
;;; It returns a binding list, and is not destructive.
;;;
;;;]

(defun add-binding (var val bindings)
  (if (eq '_ var)
      bindings
      (cons (list var val) bindings)))

;;;[
;;;
;;; <function (variable? thing)>
;;;
;;; Returns t if [[thing]] is a variable, else returns nil.
;;;
;;;]

(defun variable? (thing)
  (or (and (listp thing)
           (equal (car thing) '*var*))
      (and (symbolp thing)
           (equal (char (symbol-name thing) 0)
                  #\?))))

;;;[
;;;
;;; <function (varname var)>
;;;
;;; Returns the name of the variable [[var]].
;;;
;;;]

(defun varname (var)
  (cond
    ((and (consp var)
          (consp (cdr var)))
     (cadr var))
    ((equal (char (string var) 0) #\?)
     (intern  (string-left-trim '(#\?) (string var))
	      (find-package *intern-package*)))))




;;;[
;;; <function (make-var var)>
;;;
;;;   Create a new variable whose name is [[var]].  If [[var]] is [[x1]],
;;;   for example, the new one will be [[?X1]].
;;;
;;;]

(defun make-var (var)
  (intern (concatenate 'string "?" 
		       (cond
			((stringp var) var)
			(t (symbol-name var))))))


;;;<define name="symbol-generator">
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;[
;;;;; [[* Symbol Generation *]]
;;;;;
;;;;; Lisp has a basic facility to generate new symbols, [[gensym]].  However,
;;;;; although you can specify a different basename (so all your new symbols
;;;;; don't come out as GEN101, GEN102, etc.), there are two problems.  First,
;;;;; all symbols draw from the same numeric sequence.  So:
;;;;; \begin{verbatim}
;;;;;     > (gensym)
;;;;;     #:G913
;;;;;     > (gensym "FOO")
;;;;;     #:FOO914
;;;;; \end{verbatim}
;;;;; when you might rather have [[FOO1]] as your first ``FOO''-based
;;;;; variable.  
;;;;;
;;;;; The second problem is more serious: the symbols returned exist, but are
;;;;; not interned in any symbol table.  This means you can use them, but you
;;;;; can't refer to them by name anywhere.
;;;;;
;;;;; To avoid this, I include here some code cribbed from our MaineSAIL
;;;;; Utilities.  This implements a [[SymbolGenerator]] class to keep track of
;;;;; prefixes for new symbols and keep separate counts for them, as well as
;;;;; interning them (in [[*intern-package*]]).
;;;;;]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;</define>



;;;[
;;;
;;; <class SymbolGenerator>
;;;
;;;  Holds the state of symbol generation.  [Copied from Utilities, where a
;;;  class was needed; if doing this from scratch for this class, I'd have
;;;  used just a hash table.]
;;; 
;;;]

(defclass SymbolGenerator ()
  ((counterTable :initform (make-hash-table :test #'equal))))

;;;[
;;; <function (newSymbol \&optional prefix \&key package (intern t))>
;;;
;;;   This returns a unique symbol based on [[prefix]], which can be a string or 
;;;   a symbol.  [[package]] controls where the thing is interned, and
;;; [[intern]] determines if it is at all.
;;;
;;;]
;;; Author: rmt (Roy Turner) - Thu Nov  5 09:14:39 1998
;;; Modifications:  Removes trailing numbers, checks for uniqueness.
;;;

(defun newSymbol (&optional prefix &key (package) (intern t))
  (with-slots (counterTable) *symbolGenerator*
   (let (num sym) 
     (cond
       ((symbolp prefix)
	;; convert to string, call again:
	(newSymbol (symbol-name prefix) :package package :intern intern))
       ((stringp prefix)
	;; get rid of trailing numerals:
	(setq prefix (string-right-trim "0123456789" prefix))
	;; try new symbol names until we find one that is not in use:
	(loop do
	      (cond
	       ((setq num (gethash prefix counterTable))
		;; number exists for this prefix -- new number is just incremented
		;; one: 
		(setq num (1+ num))
		(setf (gethash prefix counterTable) num))
	       (t
		;; no number yet:
		(setf (gethash prefix counterTable) 1)
		(setq num 1)))
	    until (not (find-symbol
			(setq sym (string-append prefix 
						 (princ-to-string num))))))
	;; found one, create the symbol...
	(setq sym (make-symbol sym))
	(when intern			     ;then intern the symbol:
	  (setq sym (if package
		      (intern (format nil "~a~s" prefix num) package)
		      (intern (format nil "~a~s" prefix num)))))
	sym)
       (t
	;; then can't do any better than regular old gensym:
	(gensym))))))



;;;[
;;; 
;;; This is a bit of a kludge, creating a symbol generator here, rather than
;;; having you do it in your program.  Of course, this really should all be a
;;; separate file, say symbol-generator.lisp, with unify.lisp creating an
;;; instance of [[symbolGenerator]].
;;;
;;;]

(defvar  *SymbolGenerator* (make-instance 'SymbolGenerator))

;;;[
;;; <function (instantiate thing bindings \&key (if-unbound :first))>
;;;
;;; This ``instantiates'' a thing, for example, a clause or literal, by
;;; replacing all variables with their bindings.  To see how unbound variables
;;; are handled, see the documentation for [[instantiate-variable]].
;;;
;;;]

(defun instantiate (thing bindings &key (if-unbound :first))
  (cond
   ((variable? thing)
    (instantiate-variable thing bindings :if-unbound if-unbound))
   ((atom thing)
    thing)
   (t
    (cons (instantiate (car thing) bindings :if-unbound if-unbound)
	  (instantiate (cdr thing) bindings :if-unbound if-unbound)))))
    
;;;[
;;; <function (instantiate-variable var bindings \&key (if-unbound :first))>
;;;
;;; This will instantiate a variable against a set of bindings.  This means
;;; that if the variable is bound to another variable, that variable's binding
;;; will be chased down, etc., until a value is found.
;;;
;;; The keyword parameter [[if-unbound]] determines what happens if the
;;; variable is unbound, or if it is bound to a variable that, ultimately, is
;;; unbound.  If the value is [[:first]], then the first variable is left in
;;; the expression; if [[:last]], then the last variable found is left.  If
;;; [[nil]], then nil is returned. (Actually, whatever it is other than :first
;;; or :last is returned -- so you can have it return, e.g., :unbound, if you
;;; like).
;;;  
;;; For example, suppose we have these bindings:
;;; \begin{verbatim}
;;;     b = ((?x 2) (?y ?x) (?z ?a)).  
;;; \end{verbatim}
;;; The behavior is as follows, where => means returns:
;;; \begin{verbatim}
;;;     (instantiate-variable '?x b) => 2
;;;     (instantiate-variable '?y b) => 2
;;;     (instantiate-variable '?z b) => ?z
;;;     (instantiate-variable '?z b :if-unbound nil) => nil
;;;     (instantiate-variable '?z b :if-unbound :last) => ?a
;;; \end{verbatim}
;;;
;;;]

(defun instantiate-variable (var bindings &key (if-unbound :first))
  (multiple-value-bind (found val)
      (inst-var var bindings)
    (cond
     (found val)
     ((eql if-unbound :first)
      var)
     ((eql if-unbound :last)
      (cadr val))
     (t
      if-unbound))))

;;;[
;;; [[inst-var]] is just a helper-function for [[instantiate-variable]].
;;;]

(defun inst-var (var bindings &optional (depth 0))
  (loop with deeper-var = nil 
      for binding in bindings
      do
	(when (member var binding)
	  (let (found
		(val (if (eql var (car binding) )
			 (cadr binding)
		       (car binding))))
	    (cond
	     ((not (variable? val))
	      (return (values t val)))
	     ((multiple-value-setq (found val)
		(inst-var val
			  (remove binding bindings :test #'equal)
			  (1+ depth)))
	      (return (values t val)))
	     ((variable? (cadr val))
	      (when (or (null deeper-var)
		      (> (car val) (car deeper-var)))
		(setq deeper-var val))))))
      finally
	;; if we get here, we haven't returned from the things above --
	;; meaning we haven't found var in bindings at all!  In this case, we
	;; need to return the variable itself as the value, though noting that
	;; we haven't found a real binding.
	(return (values nil 
			  (if (or (null deeper-var)
				(<= (car deeper-var) depth))
			    (list depth var)
			  deeper-var)))))
    
		     
	     
		       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; 
;;;;; Here is the introduction to the printed version of this document, which
;;;;; is extracted from here and put where it belongs by LP/Lisp.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;<define name="intro">
#|<doc>

\section{Introduction}

This is a version of the unify algorithm written for use by students in
UMaine's COS 140 (Introduction to Artificial Intelligence) class.  It is a
general-purpose unifier, but it is especially targeted for unification of
predicate calculus expressions.  It is based on the unify functions in Winston
and Horn's early AI book.

The primary entry point to these functions is [[unify]].  [[unify]] has two
required arguments---expressions----and one optional argument--a unifier (or
\hi{binding list}).  It returns two values (using [[values]]).  The first
is either T or nil, depending on whether or not the unification was
successful.  The second is a unifier (or updated unifier, if the optional
argument was given) resulting from the unification (if the unification was
successful; else the value of the optional argument is returned).

A binding list (unifier) has the form of an association list:
\begin{alltt}
    ((\hi{var} \hi{val})
     (\hi{var} \hi{val})
     ...)
\end{alltt}
where \hi{val} can also be a variable.  In the case of a variable bound to
another, the order they occur within a binding makes no difference.

Variables in expressions are represented as atoms whose symbol name begins
with ``?''.  For example, [[?x]] is a variable.  Several functions are provided for
dealing with variables, for example:
\begin{itemize}
\item \texttt{(variable? \hi{thing})} -- returns t if \hi{thing} is a variable
\item \texttt{(varname \hi{var})} -- strips the leading ``?'' from \hi{var}
\item \texttt{(make-var \hi{basename})} -- creates a new variable based on
  \hi{basename} by prepending a ``?''
\item \texttt{(find-binding \hi{var} \hi{bindings})} -- returns the binding of
  the variable in the binding list, plus another value (via Lisp's [[values]]
  mechanism) to indicate whether the variable was really bound or not
\item \texttt{(add-binding \hi{var} \hi{val} \hi{bindings})} -- add a new
  binding to [[bindings]]
\item \texttt{(instantiate \hi{thing} \hi{bindings})} -- this will replace all
  variables in [[thing]] with their values from bindings; this takes an
  additional keyword argument; see the documentation for this function (below)
  for information about how to use it to handle unbound variables.
\end{itemize}

\section{Representing FOPC}

You are free to represent predicate calculus expressions any way you like, of
course.  However, a standard way is to represent them in Lisp is something
like the following:

\begin{tabular}[c]{cc}
\multicolumn{1}{c}{\underline{FOPC}}& 
\multicolumn{1}{c}{\underline{Lisp representation}}\\
$Human(Socrates) $ & \texttt{ (human socrates)}\\
$Human(x) $ & \texttt{ (human ?x)}\\
$Human(x) \wedge Roman(x) $ & \texttt{ (and (human ?x) (roman ?x))}\\
$Pompeian(x) \vee Roman(x) $ & \texttt{ (or (pompeian ?x) (roman ?x))}\\
$\neg Pompeian(x) $ & \texttt{ (not (pompeian ?x))}\\
$Bird(x) \rightarrow Flies(x) $ & \texttt{ (implies (bird ?x) (flies ?x))}\\
$\exists\ x\ Dog(x) \rightarrow Barks(x) $ & \texttt{ (exists (?x) (implies (dog ?x) barks ?x))}\\
$\forall\ x\ Human(x) \rightarrow Moral(x) $ & \texttt{ (forall (?x) (implies (human ?x) (mortal ?x)))}\\
\end{tabular}
</doc>
|#
;</define>


	     
#+:sbcl (declaim (sb-ext:unmuffle-conditions warning))

	   
