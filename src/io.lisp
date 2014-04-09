;;;; Orbsim
;; I/O functions

(defun body-to-list (body)
  "Converts a body into a list of its attributes"
  (list (x (pos body))       ; X pos
	(y (pos body))       ; Y pos
	(x (vel body))       ; X vel
	(y (vel body))       ; Y vel
	(mass body)          ; mass
	(size body)          ; size
	(sdl:r (colour body)); Red value 
	(sdl:g (colour body)); Green value
	(sdl:b (colour body)); Blue value
	(id body)))          ; Id

(defun bodies-to-list (bodies)
  "Calls body-to-list on a list of bodies"
  (mapcar #'body-to-list bodies))

(defun save-list (lst filename)
  "Saves the list given to a file with the name given"
  ; Open file
  (with-open-file (out filename 
		       ; set to output
		       :direction :output 
		       ; if exists, overwrite
		       :if-exists :supersede) 
    ; print lst to file
    (with-standard-io-syntax
      (print lst out)))) 

(defun save-bodies (filename)
  "Calls save-list and body-to-list"
  (save-list (bodies-to-list *bodies*) filename))

(defun read-list (filename)
  "Reads from a file with the name given"
  ; Open file
  (with-open-file (in filename)
    (with-standard-io-syntax
      ; return file contents
      (read in))))

(defun read-bodies (filename)
  "Reads from a file and converts the lists in the file
   into instances of the body class"
  (loop for bod in (read-list filename)
       collecting (make-instance 
		   'body
		   ; Make pos from 1st and 2nd items
		   :pos (make-instance
			 'point
			 :x (nth 0 bod)
			 :y (nth 1 bod))
		   ; Make vel from 3rd and 4th
		   :vel (make-instance
			 'point
			 :x (nth 2 bod)	
			 :y (nth 3 bod))
		   ; mass and size from 5th and 6th
		   :mass (nth 4 bod)
		   :size (nth 5 bod)
		   ; Colour from 6th, 7th & 8th
		   :colour (sdl:color :r (nth 6 bod)
				      :g (nth 7 bod)
				      :b (nth 8 bod))
		   ; Id from 9th item in list
		   :id (nth 9 bod))))


