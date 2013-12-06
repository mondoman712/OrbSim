(asdf:operate 'asdf:load-op :lispbuilder-sdl)

(defclass point () 
  ((x :type number
      :accessor point-x
      :initarg :x
      :initform 0)
   (y :type number
      :accessor point-y
      :initarg :y
      :initform 0)))

(defclass quad ()
  ((pt :type point
        :accessor quad-pt
        :initarg :pt)
   (len :type number
        :accessor quad-len
        :initarg :len)))

(defclass body ()
  ((pos :type point
        :accessor body-pos)
   (vel :type point)
   (frc :type point)
   (mass :type number
         :accessor body-mass)
   (qd :type quad)))

(defclass bh-tree ()
  ((body :type body)
   (quad :type quad
         :initarg :qd)
   (nw :type bh-tree)
   (ne :type bh-tree)
   (sw :type bh-tree)
   (se :type bh-tree)))

(defun make-point (x y)
  (make-instance 'point :x x :y y))

(defun largest (&rest numbers)
  "Returns the largest of the inputs"
  (loop for i in numbers maximizing i))

(defun smallest (&rest numbers)
  "Returns the smallest of the inputs"
  (loop for i in numbers minimizing i))

(defun betweenp (a x y)
  "Returns t if a is between x and y"
  (and (< a (largest x y)) (>= a (smallest x y))))

(defun get-quad-pt (qd x)
  "Returns the x or y coordinate from a quad"
  (if x
    (point-x (quad-pt qd))
    (point-y (quad-pt qd))))

(defun contains (pt qd)
  "Returns t if pnt is within the boundaries of qd"
  (let ((x (get-quad-pt qd t))
        (y (get-quad-pt qd nil)))
    (and (betweenp (point-x pt) x (+ x (quad-len qd)))
         (betweenp (point-y pt) y (+ y (quad-len qd))))))

(defun mass-sum (&rest bodies)
  (loop for body in bodies
        summing (body-mass body) into m
        summing (* (point-x (body-pos body)) (body-mass body)) into x
        summing (* (point-y (body-pos body)) (body-mass body)) into y
        return (list (make-point (/ x m) (/ y m)) m)))

(defun bhtree (q)
  (make-instance 'bh-tree :qd q))

