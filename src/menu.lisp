;;;; Orbsim
;; Menu functions

(ql:quickload 'ltk)

(define-condition not-valid-int (error)
  ((val :initarg val :reader val)))

(defun parse-int (str)
  (if (parse-integer str)
      (parse-integer str)
      (error 'not-valid-int str)))

(defun menu ()
  (ltk:with-ltk ()
    (let* ((exit (make-instance 'ltk:button
				:master nil
				:text "exit"
				:command (lambda () 
					   (setf *quit* 't)
					   (setf ltk:*exit-mainloop* 't))))
	   (frame (make-instance 'ltk:frame
				 :master nil))
	   (label-ad (make-instance 'ltk:label
				    :master frame
				    :text "add-body"
				    :anchor :n))
	   (in-posx (make-instance 'ltk:entry
				   :master frame
				   :text "0"))
	   (in-posy (make-instance 'ltk:entry
				   :master frame
				   :text "100000000"))
	   (in-velx (make-instance 'ltk:entry
				   :master frame
				   :text "1000000"))
	   (in-vely (make-instance 'ltk:entry
				   :master frame
				   :text "0"))
	   (in-size (make-instance 'ltk:entry
				   :master frame
				   :text "2"))
	   (in-id (make-instance 'ltk:entry
				 :master frame
				 :text "nil"))
	   (label-px (make-instance 'ltk:label
				    :master frame
				    :text "pos-x"))
	   (label-py (make-instance 'ltk:label
				    :master frame
				    :text "pos-y"))
	   (label-vx (make-instance 'ltk:label
				    :master frame
				    :text "vel-x"))
	   (label-vy (make-instance 'ltk:label
				    :master frame
				    :text "vel-y"))
	   (label-sz (make-instance 'ltk:label
				    :master frame
				    :text "size"))
	   (label-id (make-instance 'ltk:label
				    :master frame
				    :text "id"))
	   (sub (make-instance
		 'ltk:button :master frame :text "submit"
		 :command (lambda ()
			    (apply #'add-body
				   (append
				    (mapcar #'parse-int
					    (list
					     (ltk:text in-posx)
					     (ltk:text in-posy)
					     (ltk:text in-velx)
					     (ltk:text in-vely)))
				    (list :size (parse-int (ltk:text in-size))
					  :id (ltk:text in-id)
					  :colour 'a)))))))
    (progn
      (ltk:grid exit 2 1 :padx 3 :pady 3)
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
      (ltk:grid label-ad 1 1)
      (ltk:grid frame 1 1)))))
