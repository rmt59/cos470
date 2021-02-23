(in-package cl-user)

(defclass priority-queue ()
  ((queue :initform nil)
   (compare-function :initarg :compare-function
		     :initform #'<)))

(defmethod insert-thing ((self priority-queue) thing queue &key (compare #'<))
  (cond
   ((null queue)
    (setq queue (list thing)))
   ((funcall compare thing (car queue))
    ;; less than, should go here:
    (setf (cdr queue) (cons (car queue) (cdr queue)))
    (setf (car queue) thing))
   ((null (cdr queue))
    (setf (cdr queue) (list thing))
    queue)
   (t (insert-thing self thing (cdr queue) :compare compare)))
  queue)

(defmethod enqueue ((self priority-queue) thing)
  (with-slots (compare-function queue) self
    (setq queue (insert-thing self thing queue :compare compare-function))))


(defmethod dequeue ((self priority-queue))
  (with-slots (queue) self
    (pop queue)))

(defmethod print-queue ((self priority-queue))
  (with-slots (queue) self
    (format t "~s~%" 
	    (mapcar #'(lambda (a) (with-slots (cost coord) a
				    (list :node a :coord coord :cost cost)))
		    queue))))


;; SAMPLE NODE AND CALLS:
(defvar *sample* nil)

(defclass sample-node ()
  ((coord :initarg :coord :initform nil)
   (cost :initarg :cost :initform 0 :accessor cost)
   (parent :initarg :parent :initform nil)))

(defmethod cmp-nodes ((node1 sample-node) (node2 sample-node))
  (< (cost node1) (cost node2)))

(setq *sample* (make-instance 'priority-queue :compare-function #'cmp-nodes))

(loop for i from 1 to 10 
    do (enqueue *sample* (make-instance 'sample-node :cost (random 100) :coord (list i i)))
       (with-slots (queue) *sample*
	 (format t "Added ~s, queue contains:  " (list i i))
	 (print-queue *sample*)))



