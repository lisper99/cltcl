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

;;-----------------------------------------------------------------------------
;; A clTcl example: a viewer for ASDF
;;
;; Paul Griffioen 2009
;;-----------------------------------------------------------------------------

(in-package "CL-USER")

(defvar *INSTALLED-SYSTEMS* ()
  "A snapshot of all installed ASDF systems.")

(defvar *DEPENDS-ON-CACHE* (MAKE-HASH-TABLE)
  "A hash-table with value (ASDF-DEPENDS-ON X) for every system X in
  *INSTALLED-SYSTEMS*.")

(defvar *USED-BY-CACHE* (MAKE-HASH-TABLE)
  "A hash-table with value (ASDF-USED-BY X) for every system X in
  *INSTALLED-SYSTEMS*.")

(defun ASDF (&KEY (INTERPRETER CLTCL:*INTERPRETER*) OPTIONS)
  "Runs the ASDF viewer."
  (handler-bind 
      ((condition (lambda (error)
		    (loop 
		       with restarts = (compute-restarts error)
		       with text = (format nil "Error:~%~A~2%Restarts:" error)
		       for restart in restarts
		       for i = 1 then (+ i 1)
		       do (setf text (format nil "~A~%~A - ~A" text i restart))
		       collect i into indices
		       finally (invoke-restart
				(nth (read-from-string
				      (apply #'cltcl:call 
					     "tk_dialog" ".topHandler" 
					     "Unexpected error" text
					     nil 0 indices))
				     restarts))))))
    (cltcl:event-loop (asdf-viewer-script)
		      :interpreter interpreter
		      :options options)))

(defun ASDF-VIEWER-SCRIPT () 
  "The Tcl/Tk script for the graphical user interface."
  #TCL[

  package require Tk
  namespace import ::cltcl::callLisp

  wm protocol . WM_DELETE_WINDOW {exit}
  wm title . "ASDF viewer"
  
  proc main {} {
      
      # The toplevel widgets
      text .info -relief flat -takefocus 0 -state disabled \
        -height 10 -width 50 -yscrollcommand [list .infovbar set]
      scrollbar .infovbar -orient vertical -command [list .info yview]
      text .script -height 10 -width 80  -yscrollcommand [list .scriptvbar set]
      scrollbar .scriptvbar -orient vertical -command [list .script yview]
      frame .systems
      frame .actions
      button .close -text "Close" -width 10 -command {exit}

      # Display the widgets
      grid .systems .info .infovbar -sticky news
      grid ^        .actions -sticky nws
      grid ^        .script .scriptvbar -sticky news
      grid .close -columnspan 2
      grid columnconfigure . 1 -weight 1
      grid rowconfigure . 0 -weight 1

      # Widgets for the system list are grouped in .systems
      listbox .systems.names -width 25 -height 20 \
          -selectmode single \
          -yscrollcommand [list .systems.vbar set]
      scrollbar .systems.vbar \
          -orient vertical \
          -command [list .systems.names yview]
      bind .systems.names <<ListboxSelect>> {
          set selection [.systems.names curselection]
          callLisp on-asdf-viewer-selection-change $selection
      }
      
      # Grid the widgets in the .systems frame
      grid .systems.names .systems.vbar -sticky news
      grid rowconfigure .systems 0 -weight 1

      # The buttons are grouped in .actions
      button .actions.refresh -text "Refresh" -width 10 -command {
        callLisp on-asdf-viewer-refresh
      }
      button .actions.load -text "Load" -width 10 -command {
        set selection [.systems.names curselection]
	if {[llength $selection] == 0} {
	  setInfo "No system selected"
	} else {
	  runThreaded "Compiling" {
	    callLisp on-asdf-viewer-load $selection
	  }

	}
      }
      button .actions.run -text "Run" -width 10 -command {
        set selection [.systems.names curselection]
	if {[llength $selection] == 0} {
	  setInfo "No system selected"
	} else {
	  runThreaded "Running" {
	    callLisp on-asdf-viewer-run $selection
	  }
	}
      }
      
      # Grid the widgets in the .buttons frame
      grid .actions.refresh .actions.load .actions.run -sticky w
  } 

  proc runThreaded {title command} {

    toplevel .dialog -borderwidth 4
    wm transient .dialog .
    wm title .dialog $title
    wm protocol .dialog WM_DELETE_WINDOW {
      destroy .dialog
      callLisp kill-thread
    }

    button .dialog.abort -text "Abort thread" -command {
      destroy .dialog
      callLisp kill-thread
    }
    pack .dialog.abort
	  
    raise .dialog .
    grab .dialog
    tkwait visibility .dialog

    after idle {
      setBusy 1
      beat
    }
    uplevel 1 [list after idle $command]

    tkwait window .dialog
    setBusy ""
  }

  proc beat {} {
    if {[winfo exists .dialog]} {
      after 500 beat
      .info configure -state normal
      .info insert end "."
      .info configure -state disabled
    }
  }

  proc setSystemList {systems} {
      .systems.names delete 0 end
      foreach sys $systems {
          .systems.names insert end $sys
      }
  }

  proc setInfo {text} {
      clearInfo
      .info configure -state normal
      .info insert 0.0 $text
      .info configure -state disabled
  }

  proc clearInfo {} {
      .info configure -state normal
      .info delete 0.0 end
      .info configure -state disabled
  }
  
  # Tcl/Tk 8.6 has tk_busy command
  proc setBusy {on} {
      if {$on == {}} {
          set cursor {}
      } else {
          set cursor watch
      }
      foreach widget {.systems .info .script .close \
                      .actions.refresh .actions.load .actions.run} {
          $widget configure -cursor $cursor
      }
      update
  }

  # Run the application
  main

  # Set default script
  .script insert 0.0 "(lambda (system)
  (asdf:operate 'asdf:load-op system))"

  # Display instructions
  setInfo "The list displays all currently installed systems.

Buttons:
  Refresh  update the list of systems;
  Load     load the selected system;
  Run      run the function below.

The default function below is what Load does."

  ])

(defun ASDF-VIEWER-SET-INFO (CONTROL-STRING &REST FORMAT-ARGUMENTS)
  "Sets the text in the ASDF-VIEWER's info widget. Arguments are like
function FORMAT."
  (cltcl:post "setInfo" (apply #'format nil control-string format-arguments)))

(defmacro WITH-HOURGLASS (&BODY BODY)
  "Executes BODY while an hourglass is present. Assumes a clTcl
event-loop is active."
  `(progn 
     (clTcl:call "setBusy" t)
     (unwind-protect (progn ,@body)
       (clTcl:call "setBusy" nil))))

;; ----------------------------------------------------------------------------
;; Event handlers
;; ----------------------------------------------------------------------------

(defun ON-ASDF-VIEWER-LOAD (INDEX)
  "Event handler for the Load button."
  (let ((system (nth (read-from-string index) *installed-systems*)))
    (run-thread (lambda () 
		  (asdf:operate 'asdf:load-op system)
		  (asdf-viewer-set-info "Loaded system ~A" 
					(asdf:component-name system))))))

(defvar *THREAD*)

(defun KILL-THREAD ()
  #+SBCL(sb-thread:destroy-thread *thread*)
  #+LispWorks(mp:process-kill *thread*)
  #+cmu(mp:destroy-process *thread*)
  #+ccl(process-kill *thread*))

(defun RUN-THREAD (FUN)
  (let ((fun (let ((output *standard-output*)
		   (stream cltcl:*stream*))
	       (lambda ()
		 (let ((*standard-output* output)
		       (cltcl:*stream* stream))
		   (handler-bind 
		       ((condition (lambda (error)
				     (ignore-errors
				       (cltcl:post "setInfo"
						   (format nil "~A" error)))
				     (continue))))
		     (funcall fun))  
		   (cltcl:post "destroy" ".dialog"))))))
    (setf *thread*
	  #+SBCL(sb-thread:make-thread fun)
	  #+LispWorks(mp:process-run-function "ASDF Viewer" () fun)
	  #+cmu(mp:make-process fun :name "ASDF Viewer")
	  #+ccl(process-run-function "ASDF Viewer" fun))))

(defun ON-ASDF-VIEWER-REFRESH ()
  "Event handler for the Refresh button."
  (with-hourglass
    (refresh-asdf-systems)
    (cltcl:call "setSystemList" (mapcar #'asdf:component-name
					*installed-systems*))
    (asdf-viewer-set-info "Refreshed the system list. Found ~a systems"
			  (length *installed-systems*))))

(defun ON-ASDF-VIEWER-RUN (INDEX)
  "Event handler for the Run button."
  (let ((system (nth (read-from-string index) *installed-systems*))
	(code (cltcl:call ".script" "get" "1.0"
			  (list "end" "-" "1" "chars"))))
    (run-thread (lambda ()
		  (funcall (compile nil (read-from-string code)) system)
		  (asdf-viewer-set-info "Finished script for system ~A"
					(asdf:component-name system))))))

(defun ON-ASDF-VIEWER-SELECTION-CHANGE (NEW-INDEX)
  "Event handler for the system list widget. Called when the user
clicks on a system."
  (let ((system (nth (read-from-string new-index) *installed-systems*)))
    (asdf-viewer-set-info
     "Name:        ~A
Version:     ~A
Pathname:    ~A
Depends on:  ~{~A~^ ~}
Used by:     ~{~A~^ ~}"
     (asdf:component-name system)
     (ignore-errors (asdf:component-version system))
     (asdf:component-pathname system)
     (or (mapcar #'asdf:component-name (gethash system *depends-on-cache*))
	 '(-))
     (or (mapcar #'asdf:component-name (gethash system *used-by-cache*))
	 '(-)))))

;; ----------------------------------------------------------------------------
;; Some ASDF related functions
;; ----------------------------------------------------------------------------

(defun REFRESH-ASDF-SYSTEMS ()
  "Finds all ASDF systems and updates the global variables that
maintain a snapshot of installed ASDF systems."
  (setf *installed-systems* (all-systems-in-central-registry))
  (loop
     initially (setf *depends-on-cache* (make-hash-table))
     for x in *installed-systems*
     do (setf (gethash x *depends-on-cache*) (asdf-depends-on x)))
  (loop
     initially (setf *used-by-cache* (make-hash-table))
     for x in *installed-systems* 
     do (loop 
	   for y in (gethash x *depends-on-cache*) 
	   do (setf (gethash y *used-by-cache*)
		    (adjoin x (gethash y *used-by-cache*))))))

(defun ALL-SYSTEMS-IN-CENTRAL-REGISTRY ()
  "Returns a list of all systems in ASDF's central registry. Based on
function LIST-ALL-SYSTEMS-IN-CENTRAL-REGISTRY from swank."
  (remove-duplicates
   (loop
      for dir in asdf:*central-registry*
      for defaults = (eval dir)
      when defaults
      append (loop 
		for path in (directory
			     (make-pathname :defaults defaults
					    :version :newest
					    :type "asd"
					    :name :wild
					    :case :local))
		for system = (with-simple-restart 
				 (continue "Skip system ~A." 
					   (pathname-name path))
			       (asdf:find-system (pathname-name path)))
		when system collect it))))

(defun ASDF-DEPENDS-ON (SYSTEM)
  "List of ASDF systems on which SYSTEM depends."
  (mapcar #'asdf:find-system
	  (cdr (assoc 'asdf:compile-op
		      (asdf:component-depends-on 
		       (make-instance 'asdf:compile-op)
		       system)))))

(defun ASDF-USED-BY (SYSTEM)
  "List of ASDF systems that use SYSTEM."
  (remove-if-not (lambda (x) (member system (asdf-depends-on x)))
		 (all-systems-in-central-registry)))
