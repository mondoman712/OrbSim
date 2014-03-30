ql:quickload 'cl-tk)

(defun init ()
  (cl-tk:with-tk ()
    #|
    (cl-tk:tcl "grid" (cl-tk:tcl[
		       "ttk::frame" ".c" :padding "3 3 12 12") 
	       :column 0 :row 0 :sticky "nwes")
    (cl-tk:tcl "grid" "columnconfigure" "." 0 :weight 1
	       "grid" "rowconfigure" "." 0 :weight 1)
    |#
    (cl-tk:tcl "grid [ttk::frame .c -padding {3 3 12 12}] -column 0 -row 0 -sticky news")
    (cl-tk:tcl "grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1")
    (cl-tk:tcl "grid" (cl-tk:tcl[ 
		       "ttk::button" ".c.exit" :text "yolo"
		       :command (cl-tk:event-handler #'cl-tk:destroy))
	       :column "1" :row "1" :sticky "e")
    (cl-tk:tcl "grid" (cl-tk:tcl[
		       "ttk::button" ".c.b" :text "save"
		       :command (save-bodies "a.txt"))
	       :column "1" :row "2" :sticky "e")
    (cl-tk:tcl "grid" (cl-tk:tcl[
		       "ttk::button" ".c.b" :text "load"
		       :command (read-bodies "a.txt"))
	       :column "2" :row "1" :sticky "e")
    (cl-tk:mainloop)))
