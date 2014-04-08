;;;; Orbsim
;; Menu functions

; Load required libraries
(ql:quickload 'ltk)

(defun remove-nth (n list)
  "Removes the nth items from a list"
  (if (or (zerop n) (null list))
      (cdr list)
      (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun error-message (message &optional (button-text "OK") button2-text)
  "Makes a box appear with the message given"
  (ltk:with-ltk ()
    (let (; Label that displays the message
	  (label (make-instance 'ltk:label 
				:text message))
	  ; Button that displays the text given, or OK if none is given
	  (button (make-instance 'ltk:button 
				 :text button-text
				 :command (lambda ()
					    (setf ltk:*break-mainloop* t))))
	  (button2 (make-instance 'ltk:button
				  :text button2-text
				  :command (lambda ()
					     (error 'extreme-input :str 'nil)
					     (setf ltk:*break-mainloop* t)))))
      ; Place the items into the window
      (ltk:pack label)
      (when button2-text
	(ltk:pack button2))
      (ltk:pack button)
      (ltk:mainloop))))

(define-condition invalid-input (error)
  ((str :initarg :str :reader str)))

(define-condition extreme-input (error)
  ((str :initarg :str :reader str)))

(defun parse-int (str &key min max)
  "Converts a string to an int, and creates an error message if its invalid"
  (let ((int (parse-integer str :junk-allowed 't)))
    ; If parse-integer returns a value, return it,
    ;  else create an error message
    (if int
	(cond ((and min max) (if (and (> int min)
				      (< int max))
				 int
				   (error-message "This input could cause errors,
                                            would you like to continue?" "Yes" "No")))
	      (min (if (> int min)
		       int
		       (error-message "This input could cause errors,
                                          would you like to continue?" "Yes" "No")))
	      (max (if (< int max)
		       int
		       (error-message "This input could cause errors,
                                          would you like to continue?" "Yes" "No")))
	      ('t int))
	(progn (error-message "Please enter a valid integer")
	       (error 'invalid-input)))))
	  
(defun listbox-update (listbox)
  "Updates the listbox in remove body"
  (ltk:listbox-clear listbox)
  (ltk:listbox-append listbox 
		      ; The list of body ids, execpt for the first
		      ; (we don't want anyone removing the sun)
		      (cdr (mapcar #'id *bodies*))))

(defun menu ()
  (ltk:with-ltk ()
    (let* (; Exit button
	   (exit (make-instance 'ltk:button
				:master nil
				:text "exit"
				:command (lambda () 
					   (setf *quit* 't)
					   (setf ltk:*exit-mainloop* 't))))
	   ; Frame for add body
	   (frame (make-instance 'ltk:frame 
				 :master nil))
	   ; Frame for remove body
	   (frame2 (make-instance 'ltk:frame 
				  :master nil))
	   ; Frame for save system
	   (frame3 (make-instance 'ltk:frame 
				  :master nil))
	   ; List box for remove body
	   (bod-list (make-instance 'ltk:listbox
				    :master frame2))
	   ; Label for add body
	   (label-ad (make-instance 'ltk:label
				    :master frame
				    :text "Add body"
				    :anchor :n))
	   ; Entry for X pos in add body
	   (in-posx (make-instance 'ltk:entry
				   :master frame
				   :text "0"))
	   ; Entry for Y pos in add body
	   (in-posy (make-instance 'ltk:entry
				   :master frame
				   :text "100000000"))
	   ; Entry for X vel in add body
	   (in-velx (make-instance 'ltk:entry
				   :master frame
				   :text "1000000"))
	   ; Entry for Y vel in add body
	   (in-vely (make-instance 'ltk:entry
				   :master frame
				   :text "0"))
	   ; Entry for size in add body
	   (in-size (make-instance 'ltk:entry
				   :master frame
				   :text "2"))
	   ; Entry for id in add body
	   (in-id (make-instance 'ltk:entry
				 :master frame
				 :text "nil"))
	   ; Label for X pos in add body
	   (label-px (make-instance 'ltk:label
				    :master frame
				    :text "pos-x"))
	   ; Label for Y pos in add body
	   (label-py (make-instance 'ltk:label
				    :master frame
				    :text "pos-y"))
	   ; Label for X vel in add body
	   (label-vx (make-instance 'ltk:label
				    :master frame
				    :text "vel-x"))
	   ; Label for Y vel in add body
	   (label-vy (make-instance 'ltk:label
				    :master frame
				    :text "vel-y"))
	   ; Label for size in add body
	   (label-sz (make-instance 'ltk:label
				    :master frame
				    :text "size"))
	   ; Label for id in add body
	   (label-id (make-instance 'ltk:label
				    :master frame
				    :text "id"))
	   ; Submit button for add body
	   (sub (make-instance
		 'ltk:button :master frame :text "submit" :command 
		 ; Function called when button is pressed
		 (lambda ()
		   (block submit
		     ; Calls add body with the arguments in the list given
		     (apply #'add-body
			    ; Creates one list from multiple
			    (append
			     ; Calls parse-int on all of the items in the list 
			     (mapcar #'(lambda (x)
					 (handler-case
					     (parse-int x :min 10000000
							:max 100000000)
					     (invalid-input ()
					       (return-from submit 'nil))
					     (extreme-input ()
					       (return-from submit 'nil))))
				     ; Creates list of values given
				     (list
				      (ltk:text in-posx)
				      (ltk:text in-posy)
				      (ltk:text in-velx)
				      (ltk:text in-vely)))
		      	     ; Creates list of size, id and colour
			     (list :size (handler-case 
					     (parse-int (ltk:text in-size) :max 40)
					   (invalid-input ()
					     (return-from submit 'nil))
					   (extreme-input ()
					     (return-from submit 'nil)))
				   :id (ltk:text in-id)
				   :colour 'a)))
		     ; Updates the listbox in remove body
		     (listbox-update bod-list)))))
	   ; Label for remove body
	   (label-rm (make-instance 'ltk:label
				    :master frame2
				    :text "Remove Body"))
	   ; Remove button for remove body
	   (rm (make-instance 'ltk:button
			      :master frame2
			      :text "remove" :command 
			      ;Function called when button pressed
			      (lambda ()
				; Set bodies to:
				(setf *bodies*
				 ; Remove nth item from body
				 (remove-nth 
				  (1+ 
				   (car 
				    ; Selection in listbox
				    (ltk:listbox-get-selection 
				     bod-list)))
				  *bodies*))
				; Updates the list box
				(listbox-update bod-list))))
	   ; Entry for file name input in save system
	   (in-fn (make-instance 'ltk:entry 
				 :master frame3
				 :text "bodies.txt"))
	   ; Label for save/load system
	   (label-sl (make-instance 'ltk:label
				    :master frame3
				    :text "Save/Load System"))
	   ; Button to save in save system
	   (save (make-instance 'ltk:button
				:master frame3
				:text "Save"
				; Calls save-bodies with the file name given
				:command (lambda () 
					   (save-bodies (ltk:text in-fn)))))
	   ; Button to load in save system
	   (load (make-instance 'ltk:button
				:master frame3
				:text "Load"
				; Calls read-bodies with the file name given
				:command (lambda () 
					   (read-bodies (ltk:text in-fn))))))
    (progn
      ; Puts the add body items into the window
      (ltk:grid label-ad 1 1)
      (ltk:grid sub 8 1 :padx 3 :pady 3)
      (ltk:grid in-posx 2 2 :padx 3 :pady 3)
      (ltk:grid in-posy 3 2 :padx 3 :pady 3)
      (ltk:grid in-velx 4 2 :padx 3 :pady 3)
      (ltk:grid in-vely 5 2 :padx 3 :pady 3)
      (ltk:grid in-size 6 2 :padx 3 :pady 3)
      (ltk:grid in-id 7 2 :padx 3 :pady 3)
      (ltk:grid label-px 2 1)
      (ltk:grid label-py 3 1)
      (ltk:grid label-vx 4 1)
      (ltk:grid label-vy 5 1)
      (ltk:grid label-sz 6 1)
      (ltk:grid label-id 7 1)
      (ltk:grid frame 1 1)
      
      ; Puts the remove body items into the window
      (ltk:listbox-append bod-list (cdr (mapcar #'id *bodies*)))
      (ltk:grid label-rm 1 1)
      (ltk:grid rm 2 1)      
      (ltk:grid bod-list 2 2)
      (ltk:grid frame2 2 1)
      
      ; Puts the save system items into the window
      (ltk:grid label-sl 1 1)
      (ltk:grid in-fn 2 1 :padx 3 :pady 3)
      (ltk:grid save 3 1 :padx 3 :pady 3)
      (ltk:grid load 3 2 :padx 3 :pady 3)
      (ltk:grid frame3 3 1)

      ; Puts the exit button in the window
      (ltk:grid exit 4 1 :padx 3 :pady 3)
      (ltk:mainloop)))))
