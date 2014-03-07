(ql:quickload 'cl-tk)

(defun main ()
  (cl-tk:with-tk ()
    (cl-tk:tcl "pack" (cl-tk:tcl[ "ttk::label" ".label" :text "Hello World"))
    (cl-tk:tcl "pack" (cl-tk:tcl[ "tk::button" ".exit" 
				  :text "Exit" 
				  :command (cl-tk:event-handler 
					    #'cl-tk:destroy)))
    (cl-tk:mainloop)))

(cl-tk:with-tk ()
  (cl-tk:tcl "pack" (cl-tk:tcl[ "ttk::frame" ".screen" :width "400" :height "400"
				:background "")))

(defun a ()
  (cl-tk:with-tk ()
  (cl-tk:tcl "grid [ttk::frame .c -padding '3 3 12 12'] -column 0 -row 0 -sticky nwes")
  (cl-tk:tcl "grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1")
  (cl-tk:tcl "
grid [ttk::entry .c.feet -width 7 -textvariable feet] -column 2 -row 1 -sticky we")
  (cl-tk:tcl "grid [ttk::label .c.meters -textvariable meters] -column 2 -row 2 -sticky we")
  (cl-tk:tcl "grid [ttk::button .c.calc -text 'Calculate' -command calculate] -column 3 -row 3 -sticky w")
  (cl-tk:tcl "grid [ttk::label .c.flbl -text 'feet'] -column 3 -row 1 -sticky w")
  (cl-tk:tcl "grid [ttk::label .c.islbl -text 'is equivalent to'] -column 1 -row 2 -sticky e")
  (cl-tk:tcl "grid [ttk::label .c.mlbl -text 'meters'] -column 3 -row 2 -sticky w")
  (cl-tk:tcl "foreach w [winfo children .c] {grid configure $w -padx 5 -pady 5}")
  (cl-tk:tcl "focus .c.feet")
  (cl-tk:tcl "bind . <Return> {calculate}")
  (cl-tk:tcl "proc calculate {} {  
   if {[catch {
       set ::meters [expr {round($::feet*0.3048*10000.0)/10000.0}]
   }]!=0} {
       set ::meters ''
   }
}")))
