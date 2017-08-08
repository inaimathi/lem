(in-package #:lem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basic model stuff
(defclass grid-space ()
  ((occupant :accessor occupant :initarg :occupant :initform nil)))

(defclass grid ()
  ((width :reader width :initarg :width :initform 30)
   (height :reader height :initarg :height :initform 60)
   (spaces :reader spaces :initarg :spaces :initform (make-hash-table :test 'equal))))

(defclass unit ()
  ((unit-type :reader unit-type :initarg :unit-type :initform nil)
   (state :reader state :initarg :state :initform (make-hash-table))
   (behavior
    :reader behavior :initarg :behavior
    :initform (lambda (neighborhood)
		(declare (ignore neighborhood))
		nil))))

(defmethod get-state ((thing unit) (key symbol) &optional default)
  (gethash key (state thing) default))
(defmethod set-state! ((thing unit) (key symbol) value)
  (setf (gethash key (state thing)) value))

(defmethod empty? ((grid-space grid-space))
  (null (occupant grid-space)))
(defmethod empty! ((grid-space grid-space))
  (setf (occupant grid-space) nil))

(defmethod move-to! ((b grid-space) (a grid-space))
  (setf (occupant b) (occupant a))
  (empty! a)
  nil)

(defmethod spawn-in! ((void null) (thing unit) &rest state-k/v-pairs) nil)
(defmethod spawn-in! ((grid-space grid-space) (thing unit) &rest state-k/v-pairs)
  (let ((u (make-instance
	    'unit :behavior (behavior thing)
	    :state (alexandria:plist-hash-table state-k/v-pairs))))
    ;; TODO - inherit state from thing
    (setf (occupant grid-space) u)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Machine definition
(defmacro define-machine (name &body body)
  (let ((neighborhood (gensym "NEIGH")))
    `(progn
       (defclass ,name (unit)
	 ((unit-type :initform ,(intern (symbol-name name) :keyword))
	  (behavior
	   :initform
	   (lambda (,neighborhood)
	     (flet ((neighbor (x y) (get-neighbor ,neighborhood x y)))
	       (let ((self (occupant (neighbor 0 0))))
		 ,@body))))))
       (defun ,name () (make-instance ',name)))))

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
(defun make-grid (w h)
  (make-array
   (list w h)
   :initial-contents
   (loop for x from 0 repeat w
      collect (loop for y from 0 repeat h
		 collect (make-instance 'grid-space)))))

(defun grid-width (grid)
  (first (array-dimensions grid)))

(defun grid-height (grid)
  (second (array-dimensions grid)))

(defun get-cell (grid x y)
  (aref grid x y))

(defun step-grid! (sim-grid)
  (let* ((ds (array-dimensions sim-grid))
	 (w (first ds)) (h (second ds)))
    (loop for y from 0 to (- h 1)
       do (loop for x from 0 to (- w 1)
	     for g = (get-cell sim-grid x y)
	     unless (empty? g)
	     do (funcall
		 (behavior (occupant g))
		 (neighborhood-of sim-grid x y))))
    nil))

;;; Neighborhoods
(defun get-neighbor (neighborhood x y)
  (cdr (assoc (cons x y) neighborhood :test #'equal)))

(defun neighborhood-of (grid x y)
  (let ((deltas (loop for x from -3 to 3 append
		     (loop for y from -3 to 3
			when (>= 4 (+ (abs x) (abs y)))
			collect (cons x y)))))
    (loop for (xd . yd) in deltas
       for new-x = (+ xd x) for new-y = (+ yd y)
       when (array-in-bounds-p grid new-x new-y)
       collect (cons (cons xd yd) (get-cell grid new-x new-y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Console display basics
(defmethod show ((u unit) &key (stream *standard-output*))
  (format stream (or (get-state u :ascii) "+")))

(defmethod show ((g grid-space) &key (stream *standard-output*))
  (if (empty? g)
      (format stream ".")
      (show (occupant g) :stream stream)))

(defun show! (sim-grid)
  (let* ((ds (array-dimensions sim-grid))
	 (w (first ds)) (h (second ds)))
    (loop for y from 0 to (- h 1)
       do (loop for x from 0 to (- w 1)
	       do (show (get-cell sim-grid x y)))
       do (format t "~%"))
    (format t "~%~%")))

;;; Console interaction basics
(defun seed! (sim-grid x y thing)
  (spawn-in! (get-cell sim-grid x y) thing))

(defun sshow! (sim-grid)
  (step-grid! sim-grid)
  (show! sim-grid))

(defun play! (sim-grid &key (delay 1))
  (loop
     do (sshow! sim-grid)
     do (sleep delay)))
