;;;; Orbsim
;; I/O functions

(defun body-to-list (body)
  "Converts a body into a list of its attributes"
  (list (x (pos body))
	(y (pos body))
	(x (vel body))
	(y (vel body))
	(mass body)
	(size body)
	(sdl:r (colour body))
	(sdl:g (colour body))
	(sdl:b (colour body))
	(id body)))

(defun bodies-to-list (bodies)
  "Calls body-to-list on a list of bodies"
  (mapcar #'body-to-list bodies))

(defun save-list (lst filename)
  "Saves the list given to a file with the name given"
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print lst out))))

(defun save-bodies (filename)
  "Calls save-list and body-to-list"
  (save-list (bodies-to-list *bodies*) filename))

(defun read-list (filename)
  "Reads from a file with the name given"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun read-bodies (filename)
  "Reads from a file and converts the lists in the file
   into instances of the body class"
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
				      :b (nth 8 bod))
		   :id (nth 9 bod))))


