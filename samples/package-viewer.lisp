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

(defun CLPV (&KEY (INTERPRETER CLTCL:*INTERPRETER*) OPTIONS)
  "Runs the Common Lisp Package Viewer."
  (cltcl:event-loop (package-viewer-script)
		    :interpreter interpreter
		    :options options))

(defun PACKAGE-VIEWER-SCRIPT () 
  "The Tcl/Tk script for the graphical user interface."
  #TCL[

  package require Tk
  namespace import ::cltcl::callLisp

  wm protocol . WM_DELETE_WINDOW {exit}
  wm title . "Common Lisp Package Viewer"
  
  proc main {} {
      
      # The toplevel widgets
      frame .packages
      frame .symbols
      frame .info
      button .close -text "Close" -width 10 -command {exit}
      grid .packages .info -sticky news
      grid .symbols  ^     -sticky news
      grid .close -columnspan 2
      grid columnconfigure . 1 -weight 1
      grid rowconfigure . 1 -weight 1

      # The .info widgets
      text .info.text -takefocus 0 -state disabled -width 80 \
        -yscrollcommand [list .info.scrollbar set]
      scrollbar .info.scrollbar -orient vertical \
        -command [list .info.text yview]
      grid .info.text .info.scrollbar -sticky news
      grid columnconfigure .info 0 -weight 1
      grid rowconfigure .info 0 -weight 1

      # The .packages widgets
      listbox .packages.list -height 10 -selectmode single \
        -yscrollcommand [list .packages.scrollbar set]
      scrollbar .packages.scrollbar -orient vertical \
        -command [list .packages.list yview]
      bind .packages.list <<ListboxSelect>> {onSelectPackage}
      grid .packages.list .packages.scrollbar -sticky news
      grid columnconfigure .packages 0 -weight 1
      grid rowconfigure .packages 0 -weight 1
      
      # The .symbols widgets
      listbox .symbols.names -width 25 -height 20 -selectmode single \
          -yscrollcommand [list .symbols.scrollbar set]
      scrollbar .symbols.scrollbar -orient vertical \
          -command [list .symbols.names yview]
      bind .symbols.names <<ListboxSelect>> {onSelectSymbol}
      grid .symbols.names .symbols.scrollbar -sticky news
      grid rowconfigure .symbols 0 -weight 1
  } 

  proc onSelectSymbol {} {
    .info.text configure -state normal
    .info.text delete 0.0 end
    set package [.packages.list get active]
    if {$package  == ""} {
      .info.text insert 0.0 "Select a package"
    } else {
      set selection  [.symbols.names curselection]
      if {$selection == ""} {
        .info.text insert 0.0 "Select a symbol"
      } else {
        set symbol [.symbols.names get $selection]
	.info.text insert 0.0 [callLisp description $package $symbol]
      }
    }
    .info.text configure -state disabled
  }

  proc onSelectPackage {} {
    set selection  [.packages.list curselection]
    if {[llength $selection] != 0} {
      .symbols.names delete 0 end
      set package [.packages.list get $selection]
      foreach sys [callLisp symbol-list $package] {
          .symbols.names insert end $sys
      }
    }
  }

  # Run the application
  main

  # Fill the package list
  foreach package [callLisp package-list] {
    .packages.list insert end $package
  }

  # Set the initial selection
  .packages.list selection set 0
  onSelectPackage
  .symbols.names selection set 0
  onSelectSymbol])

(defun PACKAGE-LIST ()
  "Sorted list of all package names."
  (sort (mapcar #'package-name (list-all-packages)) #'string<))

(defun SYMBOL-LIST (PACKAGE-NAME)
  "Sorted list of all external symbols from package named with string
PACKAGE-NAME."
  (loop
     with package = (find-package (read-from-string package-name))
     for name being the external-symbols in package
     collect name into names
     finally (return (sort names #'string<))))

(defun DESCRIPTION (PACKAGE SYMBOL)
  "Description of the symbol named by string SYMBOL from the package
named by string PACKAGE. "
  (let ((symbol (find-symbol symbol (find-package (read-from-string package)))))
    (with-output-to-string (s)
      (describe symbol s))))




