
(ql:quickload 'ltk)

(defun init ()
  (ltk:with-ltk ()
    (let ((b (make-instance 'ltk:button
			    :master nil
			    :text "Hello"
			    :command (lambda ()
				       (format t "Hello~&")))))
    (ltk:pack b))))
