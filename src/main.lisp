(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defclass point () 
  ((x :type number
      :accessor x
      :initarg :x
      :initform 0)
   (y :type number
      :accessor y
      :initarg :y
      :initform 0)))

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
         :initform 0)))

(defun make-body (&key pos-x pos-y vel-x vel-y mass)
  "A function to help create instances of the body class more easily"
  (make-instance 'body
                 :pos (make-instance 'point
                                     :x pos-x
                                     :y pos-y)
                 :vel (make-instance 'point
                                     :x vel-x
                                     :y vel-y)
                 :mass mass))

(defparameter *earth* (make-body :pos-y 1e8 :pos-x 0 
                                 :vel-x 0 :vel-y 0 :mass 5.97e24))
(defparameter *sun* (make-body :pos-x 0 :pos-y 0 :mass 1.99e30))

(defun pos2pos (pos)
  "Converts position relative to centre in km to sdl coordinates"
  (flet ((new-coord (xy)
           (+ (/ (slot-value *screen-size* xy)) 
              (/ (slot-value xy pos) 468750))))
    (sdl:point :x (new-coord 'x)
               :y (new-coord 'y))))

(defun calc-g (M r)
  (/ (* *G* M) (* r r)))

(defun dist (a b)
  "Calculates the distance between positions a and b"
  (sqrt (+ (expt (abs (- (x a) (x b))) 2)
           (expt (abs (- (y a) (y b))) 2))))

(defun ang (a b)
  "Calculates the bearing of a line between a and b"
  (tan (/ (abs (- (y a) (y b)))
          (abs (- (x a) (x b))))))

(defun main ()
  (sdl:with-init ()
    (sdl:window (x *screen-size*)
                (y *screen-size*)
                :title-caption "OrbSim Prototype v1.04e-23")
    (setf (sdl:frame-rate) 60)
    
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
       (when (sdl:key= key :sdl-key-escape)
         (sdl:push-quit-event)))
      (:idle ()  (sdl:clear-display sdl:*black*)

       (sdl-gfx:draw-filled-circle
         (pos2pos *sun-pos*)
         10 
         :color sdl:*yellow*)

       (setf (x (vel *earth*)) (+ (x (vel *earth*))
                                  (calc-g (mass *sun*)
                                             (dist (pos *earth*)
                                                   (pos *sun*)))))

       (setf (x (pos *earth*)) (- (x (pos *earth*)) (x (vel *earth*))))
        
   (setf (x (vel *earth*)) 0)
       (sdl-gfx:draw-filled-circle
         (pos2pos (pos *earth*))
         3 
         :color sdl:*blue*)

      (sdl:update-display)))))

