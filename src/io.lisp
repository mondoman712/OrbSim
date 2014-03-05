(defun body-to-list (body)
  (list :pos-x (x (pos body))
	:pos-y (y (pos body))
	:vel-x (x (vel body))
	:vel-y (y (vel body))
	:mass (mass body)
	:size (size body)
	:colour-r (sdl:r (colour body))
	:colour-g (sdl:g (colour body))
	:colour-b (sdl:b (colour body))))

(defun bodies-to-list (bodies)
  (mapcar #'body-to-list bodies))

(defun save-list (lst filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print lst out))))

(defun save-bodies (lst filename)
  (save-list (bodies-to-list lst) filename))

(defun read-list (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))
