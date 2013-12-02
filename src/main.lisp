(defclass object ()
  (name
    desc
    mass
    diameter))

(defclass planet (object)
  (pos
    direction
    speed))
