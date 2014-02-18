(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defparameter *G* 6.67e-11)
(defparameter *sun-M* 1.99e30)
(defparameter *earth-M* 5.97e24)

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

(defun make-body (&key pos-x pos-y vel-x vel-y)
  (make-instance 'body
                 :pos (make-instance 'point
                                     :x pos-x
                                     :y pos-y)
                 :vel (make-instance 'point
                                     :x vel-x
                                     :y vel-y)))

(defparameter *earth* (make-body :pos-x 1e8 :pos-y 0 :vel-x 0 :vel-y 0))

(defparameter *screen-size* (make-instance 'point :x 640 :y 480))
(defparameter *sun-pos* (make-instance 'point :x 0 :y 0))
(defparameter *earth-pos* (make-instance 'point :x 1e8 :y 0))
(defparameter i 0)
(defparameter j 0)
(defparameter *earth-vel* (make-instance 'point))

(defun mid-x (val)
  (/ (slot-value val 'x) 2))

(defun mid-y (val)
  (/ (slot-value val 'y) 2))

(defun pos2pos (pos)
  "Converts position relative to centre in km to sdl coordinates"
  (sdl:point :x (+ (mid-x *screen-size*) (/ (point-x pos) 468750))
             :y (+ (mid-y *screen-size*) (/ (point-y pos) 468750))))

(defun newpos (oldpos)
  (make-instance 'point :x (+ (point-x oldpos) 1e5) :y (point-y oldpos)))

(defun calc-g (M r)
  (/ (* *G* M) (* r r)))

(defun dist (a b)
  (sqrt (+ (expt (abs (- (point-x a) (point-x b))) 2)
           (expt (abs (- (point-y a) (point-y b))) 2))))

(defun main ()
  (sdl:with-init ()
    (sdl:window (point-x *screen-size*)
                (point-y *screen-size*)
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

       (setf (point-x *earth-pos*) 
             (- (point-x *earth-pos*) 
                (/ (calc-g *sun-M* (dist *earth-pos* *sun-pos*)) 1)))

       (sdl-gfx:draw-filled-circle
         (pos2pos *earth-pos*)
         3 
         :color sdl:*blue*)

      (sdl:update-display)))))
