;;;; Orbsim
;; main source file

; Load required libraries
(ql:quickload "lispbuilder-sdl")

(load "io.lisp")
(load "menu.lisp")

; Define Classes
(defclass point () 
  ((x :type number
      :accessor x
      :initarg :x
      :initform 0)
   (y :type number
      :accessor y
      :initarg :y
      :initform 0))
  (:documentation "Describes anything with an x and y value"))

(defclass body ()
  ((pos :type point
        :accessor pos
        :initarg :pos
        :initform 0)
   (vel :type point
        :accessor vel
        :initarg :vel
        :initform 0)
   (mass :type number
         :accessor mass
         :initarg :mass
         :initform 0)
   (size :type number
	 :accessor size
	 :initarg :size 
	 :initform 1)
   (colour :type sdl:color
	   :accessor colour
	   :initarg :colour
	   :initform sdl:*white*)
   (id :type symbol
       :accessor id
       :initarg :id))
  (:documentation "Describes a body (planet etc)"))

(defun make-body (&key pos-x pos-y vel-x vel-y mass size colour id)
  "A function to help create instances of the body class more easily"
  (make-instance 'body
                 :pos (make-instance 'point
                                     :x pos-x :y pos-y)
		 :vel (make-instance 'point
				     :x vel-x :y vel-y)
                 :mass mass
		 :size size
		 :colour colour
		 :id id))

(defparameter *G* 6.67e-11) ; Gravitational Constant
(defparameter *screen-size* (make-instance 'point :x 640 :y 640))
(defparameter *bodies* (read-bodies "bodies.txt"))

(defparameter *quit* 'nil)

(defun pos2pos (pos)
  "Converts position relative to centre in km to sdl coordinates"
  (flet ((new-coord (fn xy)
           (funcall fn (/ (slot-value *screen-size* xy) 2) 
                       (/ (slot-value pos xy) 468750))))
    (sdl:point :x (new-coord #'+ 'x)
               :y (new-coord #'- 'y))))

(defun calc-g (M r)
  "Calculates the acceleration due to gravity"
  (/ (* *G* M) (* r r)))

(defun dist (a b)
  "Calculates the distance between positions a and b"
  (sqrt (+ (expt (abs (- (x a) (x b))) 2)
           (expt (abs (- (y a) (y b))) 2))))

(defun ang (a b)
  "Calculates the bearing of a line between a and b"
  (atan (- (y a) (y b))
        (- (x a) (x b))))

(defun split-force (mag ang)
  "Splits the force into its x and y components, returning them as a point"
  (make-instance 'point :x (* mag (cos ang))
                        :y (* mag (sin ang))))

(defmacro sets (fn a b)
  "like setf, but applies fn to the values"
  `(setf ,a (funcall ,fn ,a ,b)))

(defun update-vel (body)
  "updates velocity of 'body'"
  (let ((accel (split-force 
                 (calc-g (mass (car *bodies*)) (dist (pos body) (pos (car *bodies*))))
                 (ang (pos (car *bodies*)) (pos body)))))
    (sets #'+ (x (vel body)) (x accel))
    (sets #'+ (y (vel body)) (y accel))))

(defun update-pos (body)
  "Updates the position of a body using its velocities"
  (sets #'+ (x (pos body)) (x (vel body)))
  (sets #'+ (y (pos body)) (y (vel body))))

(defun update (body)
  "Updates the position and velocity of 'body'"
  (update-vel body)
  (update-pos body))

(defun update-lst (bodies)
  "Calls 'update' on each item in a list of bodies"
  (mapc #'update bodies))

(defun draw-body (body)
  "Draws a body to the screen"
  (sdl:draw-filled-circle
   (pos2pos (pos body)) (size body) :color (colour body)))

(defun draw-bodies (bodies)
  "Calls draw-body on a list... recursivly!"
  (if bodies    ; If there is anything left in the list
      (progn (draw-body (car bodies)) ; Draw the first item
	     (draw-bodies (cdr bodies))) ; Call this function on the rest of list
      't)) ; Return true when complete

(defun col (colour)
  "converts a symbol of a colour name to an sdl colour"
  (let* ((colours (list (list 'red sdl:*red*)
			(list 'blue sdl:*blue*)
			(list 'green sdl:*green*)
			(list 'yellow sdl:*yellow*)
			(list 'white sdl:*white*)
			(list 'magenta sdl:*magenta*)))
	 (pos (position colour colours :key #'car)))
    (if pos
	(cdr (nth pos colours))
	(cadr (nth (random (length colours)) colours)))))

(defun add-body (pos-x pos-y vel-x vel-y &key (mass 10) (size 2) 
					   (colour 'a) (id 'nil))
  "Adds a body to the system, with the x and y positions and velocities,
   and optionally the mass, size and/or colour"
  (setf *bodies* (append *bodies* (list
				   (make-body :pos-x pos-x :pos-y pos-y
					      :vel-x vel-x :vel-y vel-y
					      :mass mass :size size
					      :colour (col colour)
					      :id id)))))

(defun rm-body (id)
  "Removes a body from the system"
  (setf *bodies*
	(remove-if #'(lambda (x) (eq (id x) id))
		   *bodies*)))

(defun sdl-init ()
  "Initialize SDL environment"
  (sdl:window (x *screen-size*)
              (y *screen-size*)
              :title-caption "OrbSim Prototype v1.09e-23")
    (setf (sdl:frame-rate) 60))

(defun sdl-main-loop ()
  ; Define key events
  (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
       (when (or (sdl:key= key :sdl-key-escape) 
		 (sdl:key= key :sdl-key-q))
	 (sdl:push-quit-event)))
      ; Main loop
      (:idle ()  
       (when *quit*
	 (sdl:quit-sdl))
       (sdl:clear-display sdl:*black*)
       (update-lst (cdr *bodies*))
       (draw-bodies *bodies*)
       (sdl:update-display))))

(defun main ()
  (progn 
    (sb-thread:make-thread 
     #'(lambda () (progn
		    (sdl:with-init ()
		      (sdl-init)
		      (sdl-main-loop)))))
    (menu)))


