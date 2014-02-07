
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defvar *G* 6.67)
(defvar *sun-M* 1.99e30)
(defvar *earth-M* 5.97e24)

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
