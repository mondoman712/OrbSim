(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defvar *G* 6.67)
(defvar *sun-M* 1.99e30)
(defvar *earth-M* 5.97e24)

(defclass point () 
  ((x :type number
      :accessor point-x
      :initarg :x
      :initform 0)
   (y :type number
      :accessor point-y
      :initarg :y
      :initform 0)))

(defvar *screen-size* (make-instance 'point :x 640 :y 480))
(defvar *sun-pos* (make-instance 'point :x 0 :y 0))

(defun mid-x (val)
  (/ (slot-value val 'x) 2))

(defun mid-y (val)
  (/ (slot-value val 'y) 2))

(defun pos2pos (pos)
  "Converts position relative to centre in km to sdl coordinates"
  (sdl:point :x (+ (mid-x *screen-size*) (/ (point-x pos) 468750))
             :y (+ (mid-y *screen-size*) (/ (point-y pos) 468750))))

(defun newpos (oldpos)
  )

(defun proto ()
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

      (sdl:update-display)))))
