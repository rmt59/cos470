; [[file:simulator.org::*Package setup][Package setup:1]]
(unless (find-package "SIM")
  (defpackage "SIMULATOR"
    (:use "COMMON-LISP")
    (:nicknames "SIM"))
    )

(in-package sim)
; Package setup:1 ends here

; [[file:simulator.org::*Package setup][Package setup:2]]
#+:sbcl (declaim (sb-ext:muffle-conditions style-warning))
; Package setup:2 ends here

; [[file:simulator.org::*Package setup][Package setup:3]]
(load "new-symbol")
(use-package 'sym)
(load "messages")
(shadowing-import 'msg:msg)
(use-package 'message)
; Package setup:3 ends here

; [[file:simulator.org::*Global variables][Global variables:1]]
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
      ;; included in case someone reads the prior version of the
      ;; documentation, where  had this instead of :front-sensor:
      (:forward-sensor forward-sensor)
      (:front-bump front-bump-sensor)
      (:rear-bump rear-bump-sensor)
      (:right-bump right-bump-sensor)      
      (:left-bump left-bump-sensor)))

(export '(*robot-command-map* *robot-percept-map* *directions*))
; Global variables:1 ends here

; [[file:simulator.org::*Classes][Classes:1]]
(defclass simulator ()
  (
   (world :initarg :world :initform nil)
   (time :initarg :time :initform 0)
   )
  )

(export 'simulator)

(defclass world ()
  (
   (size :initarg :size :initform '(10 10))
   (objects :initarg :objects :initform nil)
   )
  )

(export 'world)

(defclass object ()
  (
   (name :initarg :name :initform (new-symbol 'o))
   (location :initarg :location :initform '(1 1))
   (orientation :initarg :orientation :initform :north)
    )
  )

(export 'object)

(defclass robot (object)
  (
   (name :initarg :name :initform (new-symbol 'robot))
   (percept :initarg :percept :initform nil)
   (next-action :initarg :next-action :initform :nop)
   (prev-action :initarg :prev-action :initform nil)
   (prev-action-success :initarg :prev-action-success :initform nil)
   (command-map :initarg :command-map
		:initform *robot-command-map*)
   (percept-map :initarg :percept-map
		:initform *robot-percept-map*)
   )
  )

(export 'robot)
; Classes:1 ends here

; [[file:simulator.org::*Simulator methods][Simulator methods:1]]
(defmethod clear ((self simulator))
  (with-slots (world) self
    (clear world)))

(export 'clear)

(defmethod reset-simulator ((self simulator) &key clear?)
  (with-slots (time world) self
    (setq time 0)
    (when clear?
      (clear world))))

(export 'reset-simulator)

(defmethod add-obstacles ((self simulator) locations)
  (dolist (loc locations)
    (add-obstacle self loc)))

(export 'add-obstacles)
; Simulator methods:1 ends here

; [[file:simulator.org::*Simulator methods][Simulator methods:2]]
(defmethod add-obstacle ((self simulator) (object object))
  (with-slots (world) self
    (add-object world object)))

(defmethod add-obstacle ((self simulator) location)
  (with-slots (world) self
    (add-object world (make-instance 'object :name (new-symbol 'obj) :location location))))

(export 'add-obstacle)

(defmethod add-object ((self simulator) object)
  (add-obstacle self object))

(export 'add-object)

(defmethod add-random-obstacles ((self simulator) &key number (max 20) (min 1))
  (unless number
    (setq number (random (+ (- max min) 1))))
  (dotimes (i number)
    (add-random-obstacle self)))

(export 'add-random-obstacles)

(defmethod add-random-obstacle ((self simulator))
  (with-slots (world) self
    (add-object world (make-instance 'object :location (random-empty-location self)))))

(export 'add-random-obstacle)

(defun random-orientation ()
  (nth (random 4) *directions*))

(defmethod add-robot ((self simulator) &key (robot nil) 
					    (name (new-symbol 'robot))
					    (random-location t)
					    (location nil)
					    (orientation nil)
					    (random-orientation t)
					    (type 'robot))
  (with-slots (world) self
    ;; if a location is specified, either explicitly or in a robot instance,
    ;; and random location hasn't been requested, then throw an error if the
    ;; location is already occupied or out of bounds:
    (setq location (or (and random-location (random-empty-location self))
		       location
		       (and robot (location robot))
		       (random-empty-location self)))
    (when (not (empty? world location))
      (error "Can't add a robot to ~s: square is not empty." location))
    (cond 
     ((null robot)			;then create one to add
      (setq robot (make-instance type
		    :location (or location (random-empty-location self))
		    :orientation (or orientation (random-orientation)))))
     (t
      (setf (slot-value robot 'location) location)
      (setf (slot-value robot 'orientation) 
	(or (and random-orientation (random-orientation))
	    orientation
	    (slot-value robot 'orientation)))))
    (add-object world robot)
    robot))

; (defmethod add-robot ((self simulator) &key (robot nil) 
; 					    (name (new-symbol 'robot))
; 					    (random-location t)
; 					    (location nil)
; 					    (orientation nil)
; 					    (random-orientation t)
; 					    (type 'robot))
;   (with-slots (world) self
;     (when (and location (not (empty? world location)))
;       (error "Can't add a robot to ~s: square is not empty." location))
;     (cond
;      ((null robot)
;       (setq robot (make-instance type
; 		    :location (or location 
; 				  (random-empty-location self))
; 		    :orientation (or orientation 
; 				     (nth (random 4) *directions*)))))
;      (t
;       (if (and (null location) random-location)
; 	(setf (slot-value robot 'location) 
; 	  (random-empty-location self)))
;       (if (and (null orientation) random-orientation)
; 	(setf (slot-value robot 'orientation)
; 	  (nth (random 4) *directions*)))))
;     (add-object world robot)
;     robot))


(export 'add-robot)

(defmethod delete-object ((self simulator) object)
  (with-slots (world) self
    (delete-object world object)))

(export 'delete-object)

(defmethod random-location ((self simulator))
  (with-slots (world) self
    (list (+ (random (car (size world))) 1)
	  (+ (random (cadr (size world))) 1))))

(export 'random-location)

(defmethod random-empty-location ((self simulator))
  (with-slots (world) self
    (loop with loc
	do (setq loc (list (+ (random (car (size world))) 1)
			   (+ (random (cadr (size world))) 1)))
	until (empty? world loc)
	finally (return loc))))

(export 'random-empty-location)

(defmethod next-location ((self simulator) location direction)
  (case direction
    (:north (list (car location) (1+ (cadr location))))
    (:east (list (1+ (car location)) (cadr location)))
    (:south (list (car location) (1- (cadr location))))
    (:west (list (1- (car location)) (cadr location)))))

(export 'next-location)

(defmethod opposite-direction ((self simulator) direction)
  (case direction
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)))

(export 'opposite-direction)

(defmethod clockwise-direction ((self simulator) direction)
  (case direction
    (:north :east)
    (:south :west)
    (:east :south)
    (:west :north)))

(export 'clockwise-direction)

(defmethod counterclockwise-direction ((self simulator) direction)
  (opposite-direction self (clockwise-direction self direction)))

(export 'counterclockwise-direction)

(defmethod run ((self simulator) &key (for 1) (sketch-each nil))
  (dotimes (i for)
    (clock-tick self)
    (when sketch-each 
      (world-sketch self))))

(export 'run)

(defmethod clock-tick ((self simulator))
  (with-slots (world time) self
    (dmsg ".")
    (dolist (object (objects world))
      (calculate-percept self object)
      (clock-tick object)
      (take-action self object))
    (incf time)))

(defmethod find-object ((self simulator) description)
  (with-slots (world) self
    (find-object world description)))

(export 'find-object)

(defmethod remove-object ((self simulator) description)
  (with-slots (world) self
    (remove-object world description)))

(export 'remove-object)

(defmethod world-sketch ((self simulator) &key (empty-char #\.) (side-wall-char #\+)
					       (top-bottom-char #\+))

  (with-slots (world) self
    (with-slots (size) world
      (let ((w (world-array world)))
	(write side-wall-char :escape nil)
	(write (make-string (cadr size) :initial-element top-bottom-char) :escape nil)
	(write side-wall-char :escape nil)
	(fresh-line)
	(loop for j from (1- (car size)) downto 0
	    do
	      (write side-wall-char :escape nil)
	      (dotimes (i (cadr size))
		(if (null (aref w i j))
		  (write empty-char :escape nil)
		  (write (aref w i j) :escape nil)))
	      (write side-wall-char :escape nil)
	      (fresh-line))
	(write side-wall-char :escape nil)
	(write (make-string (cadr size) :initial-element top-bottom-char) :escape nil)
	(write side-wall-char :escape nil)
	(fresh-line)))))

(export 'world-sketch)

(defun create-simulator (&key (size '(10 10))
			      (num-obstacles 0)
			      (obstacle-locations nil)
			      )
  (let* ((sim (make-instance 'simulator
		:world (make-instance 'world :size size))))
    (when obstacle-locations
      (add-obstacles sim obstacle-locations))
    (unless (zerop num-obstacles)
      (add-random-obstacles sim :number num-obstacles))
    sim))

(export 'create-simulator)
; Simulator methods:2 ends here

; [[file:simulator.org::*Sensor methods][Sensor methods:1]]
(defmethod calculate-percept ((self simulator) (object object))
  )

(defmethod calculate-percept ((self simulator) (object robot))
  (with-slots (time) self
    (with-slots (name percept-map percept) object
      (dfmsg "[~s  Calculating percept for ~s]" time name)
      (setq percept 
	(loop for percept in percept-map
	    collect (list (car percept)
			  (funcall (cadr percept) self object)))))))

(defmethod forward-sensor ((self simulator) object)
  (with-slots (location orientation) object
    (with-slots (world) self
      (not (empty? world (next-location self location orientation))))))

(defmethod front-bump-sensor ((self simulator) (object robot))
  (bump-sensor self object :forward))

(defmethod rear-bump-sensor ((self simulator) (object robot))
  (bump-sensor self object :backward))

(defmethod left-bump-sensor ((self simulator) (object robot))
  (bump-sensor self object :left))

(defmethod right-bump-sensor ((self simulator) (object robot))
  (bump-sensor self object :right))

(defmethod bump-sensor ((self simulator) object which)
  (with-slots (location orientation prev-action prev-action-success) object
    (with-slots (world) self
      (and
       (eql prev-action which)
       (eql nil prev-action-success)
       (not
	(empty? world
		(next-location self
			       location 
			       (case which
				 (:forward orientation)
				 (:backward
				  (opposite-direction self orientation))
				 (:left
				  (counterclockwise-direction self orientation))
				 (:right
				  (clockwise-direction self orientation))))))))))

(export '(forward-sensor front-bump rear-bump left-bump right-bump bump-sensor))
; Sensor methods:1 ends here

; [[file:simulator.org::*Effector (actuator) methods][Effector (actuator) methods:1]]
(defmethod take-action ((self simulator) (object object))
  (vdfmsg "[~s: ignoring take-action method]" (slot-value object 'name))
  )

(defmethod take-action ((self simulator) (object robot))
  (with-slots (time) self
    (with-slots (prev-action prev-action-success next-action
		 name command-map) object
      (let ((command (cadr (assoc next-action command-map))))
	(cond
	 ((null command)
	  (warn "~s  Command ~s isn't implemented for ~s; ignoring." 
		time next-action name)
	  (setq prev-action-success nil))
	 (t
	  (fmsg "~s  ~s: Performing action ~s." time name next-action)
	  (dfmsg "[~s: calling method ~s]" name command)
	  (setq prev-action-success (funcall command self object))
	  ))
	(setq prev-action next-action)
	(setq next-action nil)
	prev-action-success))))

(defmethod do-nop ((self simulator) (object object))
  (declare (ignore self object))
  t)

(defmethod do-move-forward ((self simulator) (object object))
  (with-slots (name location orientation) object
    (move-object self object (next-location self location orientation))))

(defmethod do-move-backward ((self simulator) (object object))
  (with-slots (name location orientation) object
    (move-object self object
		 (next-location self
				location (opposite-direction self orientation)))))

(defmethod do-move-left ((self simulator) (object object))
  (with-slots (name location orientation) object
    (move-object self object
		 (next-location self
				location (counterclockwise-direction
					  self orientation)))))

(defmethod do-move-right ((self simulator) (object object))
  (with-slots (name location orientation) object
    (move-object self object
		 (next-location self location (clockwise-direction
					       self orientation)))))

(defmethod do-turn-clockwise ((self simulator) (object object))
  (turn-object self object :clockwise))

(defmethod do-turn-counterclockwise ((self simulator) (object object))
  (turn-object self object :counterclockwise))


(defmethod turn-object ((self simulator) (object object) direction)
  (declare (ignore direction))
  t)

(defmethod turn-object ((self simulator) (object robot) direction)
  (with-slots (orientation name) object
    (setq orientation (if (eql direction :clockwise)
			(clockwise-direction self orientation)
			(counterclockwise-direction self orientation)))
    (fmsg "~s: Turning right, new orientation = ~s." 
	  name orientation)
    t))

(defmethod move-object ((self simulator) object new-loc)
  (with-slots (name location) object
    (with-slots (world) self
      (cond
       ((empty? world new-loc)
	(setq location new-loc)
	(fmsg "~s: Moving to ~s." name location)
	t)
       (t
	(fmsg "~s: Tried and failed to move to ~s." name location)
	nil)))))

(export '(do-nop do-move-forward do-move-backward do-move-left
	  do-move-right do-turn-clockwise do-turn-counterclockwise 
	  turn-object move-object ))
; Effector (actuator) methods:1 ends here

; [[file:simulator.org::*World methods][World methods:1]]
(defmethod objects ((self world))
  (with-slots (objects) self
    objects))

(defmethod empty? ((self world) location)
  (with-slots (objects size) self
      (and (> (car location) 0)
	   (<= (car location) (car size))
	   (> (cadr location) 0)
	   (<= (cadr location) (cadr size))
	   (loop for obj in objects
	       when (equal (slot-value obj 'location) location)
	       return nil
	       finally (return t)))))

(defmethod in-bounds? ((self world) loc)
  (with-slots (size) self
    (and (>= (car loc) 1) (<= (car loc) (car size))
	 (>= (cadr loc) 1) (<= (cadr loc) (cadr size)))))

(defmethod add-object ((self world) object)
  (with-slots (size objects) self
    (with-slots (location name) object
      (cond
       ((not (in-bounds? self location))
	(cerror "Continue" "Can't add object ~s at ~s -- out of bounds." 
	       name location)
	nil)
       ((not (empty? self location))
	(cerror "Continue" "Can't add object ~s at ~s -- location isn't empty" 
	        name location)
	nil)
       (t (push object objects))))))

(defmethod clear ((self world))
  (with-slots (objects) self
    (setq objects nil)))

(defmethod object-locations ((self world))
  (with-slots (objects) self
    (mapcar #'(lambda (o) (copy-list (slot-value o 'location)))
	    objects)))

(defmethod size ((self world))
  (with-slots (size) self
    size))

(defmethod delete-object ((self world) object)
  (remove-object self object))



(defmethod remove-object ((self world) description)
  (with-slots (objects) self
    (let ((obj (find-object self description)))
      (when obj
	(with-slots (name) obj
	  (dfmsg "[Removing object ~s from world]" name)
	  (setq objects (remove obj objects)))))))


(defmethod find-object ((self world) (location cons))
  (with-slots (objects) self
    (car (member location objects :test #'(lambda (a b) 
					    (equal a (location b)))))))


(defmethod find-object ((self world) (location symbol))
  (with-slots (objects) self
    (car (member location objects :test #'(lambda (a b) 
					    (eql a (name b)))))))

(defmethod find-object ((self world) (object object))
  (with-slots (objects) self
    (car (member object objects))))




(defmethod world-array ((self world))
  (with-slots (size objects) self
    (let ((a (make-array size :initial-element nil)))
      (dolist (obj objects)
	(setf (aref a (1- (car (slot-value obj 'location)))
		    (1- (cadr (slot-value obj 'location))))
	  (icon obj)))
      a)))
(export '(objects empty? in-bounds? add-object clear object-locations size delete-object find-objectremove-object world-array))
; World methods:1 ends here

; [[file:simulator.org::*Object methods][Object methods:1]]
(defmethod clock-tick ((self object))
  :nop)

(defmethod name ((self object))
  (with-slots (name) self
    name))

(export 'name)

(defmethod location ((self object))
  (with-slots (location) self
    location))

(export 'location)

(defmethod orientation ((self object))
  (with-slots (orientation) self
    orientation))

(export 'orientation)

(defmethod icon ((self object))
  #\@)

(export 'icon)
; Object methods:1 ends here

; [[file:simulator.org::*Robot methods][Robot methods:1]]
(defmethod clock-tick ((self robot))
  (with-slots (percept next-action name agent-program) self
    (setq next-action (agent-program self percept))
    (dfmsg "[~s: ~s -> ~s]" name percept next-action)
    next-action
    ))

(defmethod agent-program ((self robot) percept)
  (with-slots (name percept next-action) self
    (dfmsg "[~s: current percept = ~s, next action = ~s]"
	   name percept next-action)
    (setq next-action :nop)
    ))

(export 'agent-program)


(defmethod icon ((self robot))
  (with-slots (orientation) self
    (case orientation
      (:north #\^)
      (:south #\v)
      (:east #\>)
      (:west #\<)
      (otherwise #\R))))
; Robot methods:1 ends here

; [[file:simulator.org::*Example: =random-robot=][Example: =random-robot=:1]]
(defclass random-robot (robot) ())

(export 'random-robot)

(defmethod agent-program ((self random-robot) percept)
  (with-slots (name) self 
    (let ((next-action (car (nth (random (length *robot-command-map*)) 
				 *robot-command-map*))))
      (dfmsg "[~s: percept = ~s]" name percept) 
      (dfmsg "[~s: choosing ~s as next action]" name next-action)
      next-action)))
; Example: =random-robot=:1 ends here

; [[file:simulator.org::*Example: =random-robot=][Example: =random-robot=:2]]
#+:sbcl (declaim (sb-ext:unmuffle-conditions style-warning))
; Example: =random-robot=:2 ends here
