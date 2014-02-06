
(ql:quickload "lispbuilder-sdl")

(defparameter *random-color* sdl:*white*)
(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "Move a rectangle using the mouse")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (sdl:push-quit-event))
      (:idle ()
       ;; Change the color of the box if the left mouse button is depressed
       (when (sdl:mouse-left-p)
         (setf *random-color* 
               (sdl:color :r (random 255) :g (random 255) :b (random 255))))

       ;; Clear the display each game loop
       (sdl:clear-display sdl:*black*)

       ;; Draw the box having a center at the mouse x/y coordinates.
       (sdl:draw-box 
         (sdl:rectangle-from-midpoint-* 
           (sdl:mouse-x) 
           (sdl:mouse-y) 20 20)
                     :color *random-color*)

       ;; Redraw the display
       (sdl:update-display)))))

(defun proto ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "OrbSim Prototype v1.04e-23")
    (setf (sdl:frame-rate) 60)
    
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (sdl:push-quit-event))
      (:key-down-event (:key key)
       (when (sdl:key= key :sdl-key-escape)
         (sdl:push-quit-event)))

      (sdl:clear-display sdl:*black*)
      (sdl:draw-circle-* (sdl:mouse-x) (sdl:mouse-y) 
                              20 :color sdl:*white*
                              :surface *default-surface*)
      (sdl:update-display)
      )))
