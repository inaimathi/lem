(in-package #:lem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basic model stuff
(defclass grid-space ()
  ((occupant :accessor occupant :initarg :occupant :initform nil)))

(defclass unit ()
  ((state :reader state :initarg :state :initform (make-hash-table))
   (code :reader code :initarg :code :initform nil)
   (behavior
    :reader behavior :initarg :behavior
    :initform (lambda (neighborhood)
		(declare (ignore neighborhood))
		nil))))

(defmethod unit-type ((thing unit))
  (class-name (class-of thing)))

(defmethod get-state ((thing unit) (key symbol) &optional default)
  (gethash key (state thing) default))
(defmethod set-state! ((thing unit) (key symbol) value)
  (setf (gethash key (state thing)) value))

(defmethod empty? ((void null)) nil)
(defmethod empty? ((grid-space grid-space))
  (null (occupant grid-space)))
(defmethod empty! ((void null)) nil)
(defmethod empty! ((grid-space grid-space))
  (setf (occupant grid-space) nil))

(defmethod move-to! ((b grid-space) (a grid-space))
  (setf (occupant b) (occupant a))
  (empty! a)
  nil)

(defmethod spawn-in! ((void null) (thing unit) &rest state-k/v-pairs) nil)
(defmethod spawn-in! ((grid-space grid-space) (thing unit) &rest state-k/v-pairs)
  (let ((u (make-instance
	    (class-name (class-of thing)) :code (code thing) :behavior (behavior thing)
	    :state (alexandria:plist-hash-table state-k/v-pairs))))
    ;; TODO - inherit state from thing
    (setf (occupant grid-space) u)
    nil))
(defmethod spawn-in! ((grid-space grid-space) (thing symbol) &rest state-k/v-pairs)
  (setf (occupant grid-space)
	(apply (fdefinition thing) state-k/v-pairs))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Machine definition
(defmacro define-machine (name &body body)
  (let ((neighborhood (gensym "NEIGH")))
    `(progn
       (defclass ,name (unit)
	 ((code :initform ',body)
	  (behavior
	   :initform
	   (lambda (,neighborhood)
	     (flet ((neighbor (x y) (get-neighbor ,neighborhood x y)))
	       (let* ((here (neighbor 0 0))
		      (self (occupant here)))
		 (declare (ignorable here self))
		 ,@body))))))
       (defun ,name (&rest state-k/v-pairs)
	 (make-instance ',name :state (alexandria:plist-hash-table state-k/v-pairs))))))

;;;;; Specific machines
(define-machine ray
  (spawn-in! (neighbor -1 0) self))

(define-machine line
  (let ((position (get-state self :position 0)))
    (when (> 10 position)
      (spawn-in! (neighbor -1 0) self :position (+ position 1)))
    (unless (= 0 position)
      (spawn-in! (neighbor 1 0) self :position (- position 1)))))

(define-machine box
  (let ((x (get-state self :x 0))
	(y (get-state self :y 0)))
    (when (and (> 10 x) (or (= y 0) (= y 10)))
      (spawn-in! (neighbor -1 0) self :x (+ x 1) :y y))
    (unless (= 0 x)
      (when (or (= y 0) (= y 10))
	(spawn-in! (neighbor 1 0) self :x (- x 1) :y y)))
    (when (and (> 10 y) (or (= x 0) (= x 10)))
      (spawn-in! (neighbor 0 -1) self :x x :y (+ y 1)))
    (unless (= 0 y)
      (when (or (= x 0) (= x 10))
	(spawn-in! (neighbor 0 1) self :x x :y (- y 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Grid Simulation
(defclass grid ()
  ((width :reader width :initarg :width :initform 30)
   (height :reader height :initarg :height :initform 60)
   (legend :reader legend :initarg :legend :initform (make-hash-table :test 'eql))
   (spaces :reader spaces :initarg :spaces
	   :initform (make-array
		      (list 30 60)
		      :initial-contents
		      (loop for x from 0 repeat 30
			 collect (loop for y from 0 repeat 60
				    collect (make-instance 'grid-space)))))))

;; FIXME - all of the below should expect/emit `grid` instead of a 2D array

(defun make-grid (w h)
  (let ((arr (make-array
	      (list w h)
	      :initial-contents
	      (loop for x from 0 repeat w
		 collect (loop for y from 0 repeat h
			    collect (make-instance 'grid-space))))))
    (make-instance 'grid :spaces arr :width w :height h)))

(defmethod get-cell ((grid grid) x y)
  (aref (spaces grid) x y))

(defmethod step! ((sim-grid grid))
  (loop for y from 0 repeat (height sim-grid)
     do (loop for x from 0 repeat (width sim-grid)
	   for g = (get-cell sim-grid x y)
	   unless (empty? g)
	   do (funcall
	       (behavior (occupant g))
	       (neighborhood-of sim-grid x y))))
  nil)

;;; Neighborhoods
(defparameter n*von-neumann
  '((-1 0) (1 0) (0 1) (0 -1)))
(defparameter n*moore
  (loop for x in (list -1 0 1)
     append (loop for y in (list -1 0 1)
	       collect (list x y))))
(defparameter n*extended
  (loop for x from -4 to 4 append
       (loop for y from -4 to 4
	  when (>= 4 (+ (abs x) (abs y)))
	  collect (list x y))))

(defun get-neighbor (neighborhood x y)
  (cdr (assoc (cons x y) neighborhood :test #'equal)))

(defmethod neighborhood-of ((grid grid) x y)
  (loop for (xd yd) in n*extended
     for new-x = (+ xd x) for new-y = (+ yd y)
     when (array-in-bounds-p (spaces grid) new-x new-y)
     collect (cons (cons xd yd) (get-cell grid new-x new-y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Console display basics
(defmethod ->ascii ((u unit))
  (or (get-state u :ascii) "+"))

(defmethod show ((u unit) &key (stream *standard-output*))
  (format stream (->ascii u)))

(defmethod show ((g grid-space) &key (stream *standard-output*))
  (if (empty? g)
      (format stream ".")
      (show (occupant g) :stream stream)))

(defmethod show! ((sim-grid grid))
  (loop for y from 0 repeat (height sim-grid)
     do (loop for x from 0 repeat (width sim-grid)
	   do (show (get-cell sim-grid x y)))
     do (format t "~%"))
  (format t "~%~%"))

;;; Console interaction basics
(defmethod seed! ((sim-grid grid) x y thing)
  (spawn-in! (get-cell sim-grid x y) thing))

(defmethod sshow! ((sim-grid grid))
  (step! sim-grid)
  (show! sim-grid))

(defmethod play! ((sim-grid grid) &key (delay 1))
  (loop
     do (sshow! sim-grid)
     do (sleep delay)))
