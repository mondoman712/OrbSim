(defclass object ()
  (name
    desc
    mass
    diameter))

(defclass planet (object)
  (pos
    direction
    speed))

(asdf:operate 'asdf:load-op :lispbuilder-sdl)

