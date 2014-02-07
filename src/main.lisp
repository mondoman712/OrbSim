
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defparameter *random-color* sdl:*white*)
(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "Move a rectangle using the mouse")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (sdl:push-quit-event))
;      (:idle ()
       ;; Change the color of the box if the left mouse button is depressed
;       (when (sdl:mouse-left-p)
;         (setf *random-color* 
;               (sdl:color :r (random 255) :g (random 255) :b (random 255))))
)

       ;; Clear the display each game loop
       (sdl:clear-display sdl:*black*)

        (sdl-gfx:draw-filled-circle-*
          (sdl:mouse-x)
          (sdl:mouse-y)
          20 
          :color *random-color*)

       ;; Redraw the display
       (sdl:update-display)))))

(defvar *G* 6.67)
(defvar *sun-M* 1.99e30)
(defvar *earth-M* 5.97e24)

#|
(defclass point ()
  ((x :type number
      :accessor point-x
      :initarg :x
      :initform 0)
   (y :type number
      :accessor point-y
      :initarg :y
      :initform 0)))
|#

(defun newpos (oldpos)
  )

(defun proto ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "OrbSim Prototype v1.04e-23")
    (setf (sdl:frame-rate) 60)
    
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
       (when (sdl:key= key :sdl-key-escape)
         (sdl:push-quit-event)))
      (:idle ()  (sdl:clear-display sdl:*black*)

       (sdl-gfx:draw-filled-circle-*
         320
         240
         10 
         :color sdl:*yellow*)

      (sdl:update-display)))))
