;;;; Orbsim
;; main source file

; Load required libraries
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

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
	   :initform sdl:*white*))
  (:documentation "Describes a body (planet etc)"))

(defun make-body (&key pos-x pos-y vel-x vel-y mass size colour)
  "A function to help create instances of the body class more easily"
  (make-instance 'body
                 :pos (make-instance 'point
                                     :x pos-x :y pos-y)
		 :vel (make-instance 'point
				     :x vel-x :y vel-y)
                 :mass mass
		 :size size
		 :colour colour))

(defparameter *earth* (make-body :pos-x 0 :pos-y 1e8 
				   :vel-x 1e6 :vel-y 55 
				   :mass 5.97e24 :size 3 :colour sdl:*blue*))
(defparameter *sun* (make-body :pos-x 0 :pos-y 0 
			       :mass 1.99e30 :size 10 :colour sdl:*yellow*))
(defparameter *screen-size* (make-instance 'point :x 640 :y 640))
(defparameter *G* 6.67e-11)
(defparameter *bodies* '(*sun* *earth*))

(defun pos= (a b)
  "Checks if two points are equal"
  (if (and (= (x a) (x b)) (= (y a) (y b)))
    't 'nil))

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
  (let ((accel (split-force 
                 (calc-g (mass *sun*) (dist (pos body) (pos *sun*)))
                 (ang (pos *sun*) (pos body)))))
    (sets #'+ (x (vel body)) (x accel))
    (sets #'+ (y (vel body)) (y accel))))

(defun update-pos (body)
  (sets #'+ (x (pos body)) (x (vel body)))
  (sets #'+ (y (pos body)) (y (vel body))))

(defun update (body)
  (update-vel body)
  (update-pos body))

(defun init ()
  "Initialize SDL environment"
  (sdl:window (x *screen-size*)
              (y *screen-size*)
              :title-caption "OrbSim Prototype v1.09e-23")
    (setf (sdl:frame-rate) 60))

(defun draw-body (body)
  (sdl-gfx:draw-filled-circle
   (pos2pos (pos body)) (size body) :color (colour body)))

(defun main-loop ()
  ; Define key events
  (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
       (when (or (sdl:key= key :sdl-key-escape) 
		 (sdl:key= key :sdl-key-q))
         (sdl:push-quit-event)))
      ; Main loop
      (:idle ()  
       (sdl:clear-display sdl:*black*)
       
       (update *earth*)
        
       (draw-body *sun*)
       (draw-body *earth*)

      (sdl:update-display))))

(defun main ()
  (sdl:with-init ()
    (init)
    (main-loop)))
