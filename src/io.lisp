(defun body-to-list (body)
  (list (x (pos body))
	(y (pos body))
	(x (vel body))
	(y (vel body))
	(mass body)
	(size body)
	(sdl:r (colour body))
	(sdl:g (colour body))
	(sdl:b (colour body))))

(defun bodies-to-list (bodies)
  (mapcar #'body-to-list bodies))

(defun save-list (lst filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print lst out))))

(defun save-bodies (filename)
  (save-list (bodies-to-list *bodies*) filename))

(defun read-list (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun read-bodies (filename)
  (loop for bod in (read-list filename)
       collecting (make-instance 
		   'body
		   :pos (make-instance
			 'point
			 :x (nth 0 bod)
			 :y (nth 1 bod))
		   :vel (make-instance
			 'point
			 :x (nth 2 bod)	
			 :y (nth 3 bod))
		   :mass (nth 4 bod)
		   :size (nth 5 bod)
		   :colour (sdl:color :r (nth 6 bod)
				      :g (nth 7 bod)
				      :b (nth 8 bod)))))


