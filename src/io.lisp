(defun get-slots (object)
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object))))

(defun printable-object (object)
  (cons (class-of object) 
	(loop for slot in (get-slots object)
	   collecting (slot-value object slot)))

(defun save-list (lst filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print lst out))))

(defun save-obj-list (lst filename)
  (save-list (mapcar #'printable-object lst) filename))

(defun read-list (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))
