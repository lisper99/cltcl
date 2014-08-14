;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Copyright (c) 2009 Paul Griffioen
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package "CL-USER")

(defun RUN-FOURIER-EXAMPLE (&optional (interpreter cltcl:*interpreter*))
  (cltcl:event-loop 
   #TCL[

   package require Plotchart
   namespace import ::cltcl::callLisp

   # State variables for the GUI
   set ymax 4
   set ymin -1
   set xmin -4
   set xmax 4
   set selected_harmonics {1 2 3 4 5 6 7 8}
   set nr_harmonics 32
   set plot_data {}

   # Setup the window
   wm protocol . WM_DELETE_WINDOW {exit}
   wm title . "Fourier series demo"
   wm resizable . 1 1

   # Top level widgets
   canvas .canvas -background white -width 700 -height 400
   text .function -height 5 -highlightthickness 0
   frame .buttons
   grid .canvas       -    -sticky nesw
   grid .function .buttons -sticky nesw

   # The various buttons 
   button .buttons.plot -text "Plot" -command {
     set fun [.function get 1.0 {end - 1 chars}]
     callLisp on-plot $fun
   }
   button .buttons.plotFourier -text "Fourier" -command {
     global nr_harmonics
     set fun [.function get 1.0 {end - 1 chars}]
     callLisp on-plot-fourier $fun $nr_harmonics
   }
   button .buttons.plotHarmonics -text "Harmonics" -command {
     global selected_harmonics
     set fun [.function get 1.0 {end - 1 chars}]
     callLisp on-plot-harmonics $fun $selected_harmonics
   }
   button .buttons.clear -text "Clear" -command {
     global plot_data
     set plot_data {}
     renderPlots
   }
   button .buttons.settings -text "Settings..." -command {
     run_settings_dialog
     renderPlots
   }

   # Add the buttons to the window
   grid .buttons.plot -sticky ew
   grid .buttons.plotFourier -sticky ew
   grid .buttons.plotHarmonics -sticky ew
   grid .buttons.clear -sticky ew
   grid .buttons.settings -sticky ew

   # Make the canvas resizable
   grid columnconfigure . 0 -weight 1
   grid rowconfigure . 0 -weight 1
   bind .canvas <Configure> {renderPlots}

   # An example function
   .function insert 1.0 "(lambda (x)\n  (/ (expt (mod (+ x 2) 3) 2) 4))"
   #.function insert 1.0 "(lambda (x)\n  (floor (abs x)))"

   proc plotData {name color data} {
     global plot_data
     lappend plot_data $name $color $data
     renderPlots
   }

   proc renderPlots {} {
     global xmin xmax ymin ymax plot_data
     .canvas delete all
     set plot [::Plotchart::createXYPlot .canvas \
       [list $xmin $xmax 1] [list $ymin $ymax 1]]
     foreach {name color series} $plot_data {
       $plot dataconfig $name -color $color
       foreach {x y} $series {
         $plot plot $name $x $y
       }
     }
   }

   proc run_settings_dialog {} {

     # The globals we'll modify (or not)
     global xmin xmax ymin ymax nr_harmonics selected_harmonics

     # The toplevel window that will be the dialog
     toplevel .dialog -borderwidth 4
     wm transient .dialog .
     wm title .dialog "Settings"
     wm protocol .dialog WM_DELETE_WINDOW {destroy .dialog}
     
     # Make a label/entry pair for each setting
     set settings [list 0 "Min x" $xmin \
                        1 "Max x" $xmax \
			2 "Min y" $ymin \
			3 "Max y" $ymax \
			4 "Series length" $nr_harmonics \
			5 "Harmonics" $selected_harmonics \
			6 "Plot precision" [callLisp get-plot-precision] \
			7 "Integral precision" [callLisp get-integral-precision]]
     foreach {nr text value} $settings {
       label .dialog.label$nr -text $text
       entry .dialog.entry$nr
       .dialog.entry$nr insert 0 $value
       grid .dialog.label$nr .dialog.entry$nr 
     }

     # The Okay and Cancel buttons
     button .dialog.okay -text "OK" -command {
       set settings [list 0 xmin 1 xmax 2 ymin 3 ymax \
                          4 nr_harmonics 5 selected_harmonics]
       foreach {nr variable} $settings {
         set $variable [.dialog.entry$nr get]
       }
       callLisp set-plot-precision [.dialog.entry6 get]
       callLisp set-integral-precision [.dialog.entry7 get]
       destroy .dialog
     }
     button .dialog.cancel -text "Cancel" -command {destroy .dialog}
     grid .dialog.okay .dialog.cancel 

     # Run the window as a dialog
     raise .dialog .
     grab .dialog
     tkwait window .dialog
   }
   ] :interpreter interpreter))

(defvar *PLOT-PRECISION* 0.03)
(defvar *INTEGRAL-PRECISION* 0.01)

(defun GET-PLOT-PRECISION ()
  *plot-precision*)

(defun GET-INTEGRAL-PRECISION ()
  *integral-precision*)

(defun SET-PLOT-PRECISION (PRECISION)
  (setf *plot-precision* (read-from-string precision)))

(defun SET-INTEGRAL-PRECISION (PRECISION)
  (setf *integral-precision* (read-from-string precision)))

(defun ON-PLOT (FUNCTION)
  (let ((fun (compile nil (read-from-string function))))
    (plot-function fun (- pi) pi :color "darkgray")))

(defun ON-PLOT-FOURIER (FUNCTION LENGTH)
  (let ((fun (compile nil (read-from-string function)))
	(len (read-from-string length)))
    (plot-function (fourier-series fun
				   (loop for n from 1 below len collect n))
		   (- pi) pi :color "blue")))

(defun ON-PLOT-HARMONICS (FUNCTION SELECTION)
  (let ((fun (compile nil (read-from-string function)))
	(range (mapcar #'read-from-string
		       (cltcl:read-tcl-list-from-string1 selection))))
    (loop
       for n in range
       do (plot-function (fourier-series fun (list n))
			 (- pi) pi :color "yellow")
       finally (plot-function (fourier-series fun range)
			      (- pi) pi :color "green"))))

(defun PLOT-FUNCTION (FUN FROM TO &KEY (DELTA (GET-PLOT-PRECISION)) (COLOR "BLACK"))
  (let ((name (gensym)))
    (cltcl:call "plotData" name color
		(loop
		   for x from from upto to by delta
		   collect (format nil "~,4F" x)
		   collect (format nil "~,4F" (funcall fun x))))
    name))

(defun FOURIER-SERIES (FUN RANGE)
  "Arguments RANGE must be a list of positive numbers. Calculates the
part of the Fourier series for frequencies from RANGE."
  (let ((coeffs (loop for n in range
		   collect (list n (integrate-function
				    (lambda (x)
				      (* (funcall fun x)
					 (exp (- (* (complex 0 1) n x)))))
				    (- pi) pi))))
	(base (/ (integrate-function fun (- pi) pi) 2)))
    (lambda (x)
      (realpart
       (/ (+ base
	     (loop for (n coeff) in coeffs
		summing (* coeff (exp (* (complex 0 1) n x)))))
	  pi)))))

(defun INTEGRATE-FUNCTION (FUN FROM TO &KEY (DELTA (GET-INTEGRAL-PRECISION)))
  (loop for x from from to to by delta
     summing (* delta (funcall fun x))))
