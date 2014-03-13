(ql:quickload 'qt)
(in-package :qt)
(named-readtables:in-readtable :qt)

(defclass menu () ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after
    ((instance menu) &key)
  (new instance)
  (#_setGeometry instance 200 200 300 300)
  (#_setWindowTitle instance "WORK!"))

(defun init ()
  (make-qapplication)
  (with-main-window (window (make-instance 'menu))))
