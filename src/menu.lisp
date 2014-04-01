(ql:quickload 'ltk)

(defvar a 'nil)

(defun init ()
  (ltk:with-ltk ()
    (let ((b (make-instance 'ltk:button
			    :master nil
			    :text "Hello"
			    :command (lambda ()
				       (format t "Hello ~a~&" a))))
	  (in (make-instance 'ltk:entry
			     :master nil
			     :text "yes?"
			     :exportselection a))
	  (ex (make-instance 'ltk:button
			     :master nil
			     :text "exit"
			     :command (lambda ()
					(setf ltk:*exit-mainloop* t)))))
    (progn
      (ltk:grid b 1 2)	
      (ltk:grid ex 2 2)
      (ltk:grid in 1 4)))))

