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
					   (setf ltk:*exit-mainloop* 't))))
	   (in-posx (make-instance 'ltk:entry
				   :master nil
				   :text "pos-x"))
	   (in-posy (make-instance 'ltk:entry
				   :master nil
				   :text "pos-y"))
	   (in-velx (make-instance 'ltk:entry
				   :master nil
				   :text "vel-x"))
	   (in-vely (make-instance 'ltk:entry
				   :master nil
				   :text "vel-y"))
	   (sub (make-instance
		 'ltk:button :master nil :text "submit"
		 :command (lambda ()
			    (apply #'add-body
				   (mapcar #'parse-int
					  (list
					   (ltk:text in-posx)
					   (ltk:text in-posy)
					   (ltk:text in-velx)
					   (ltk:text in-vely)))))))
;	   (bod-list (mapcar #'id *bodies*))
;	   (list (make-instance 'ltk:listbox
;				:master nil
;				:listvariable bod-list)))
	   (spin (make-instance 'ltk:spinbox
				:master nil))
)
    (progn
      (ltk:grid exit 2 1 :padx 3 :pady 3)
      (ltk:grid sub 1 1 :padx 3 :pady 3)
      (ltk:grid in-posx 1 2 :padx 3 :pady 3)
      (ltk:grid in-posy 2 2 :padx 3 :pady 3)
      (ltk:grid in-velx 3 2 :padx 3 :pady 3)
      (ltk:grid in-vely 4 2 :padx 3 :pady 3)
 ;     (ltk:grid list 1 3 :padx 3 :pady 3)
      (ltk:grid spin 5 2)
     ))))


