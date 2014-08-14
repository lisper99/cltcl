;; Copyright (c) 2009-2014 Paul Griffioen
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
;; clTcl communication
;;
;; 0. The with-Tcl/Tk macro
;; 1. Connecting to Tcl/Tk
;; 2. Communicating with Tcl/Tk
;;
;; Paul Griffioen 2009-2014
;;-----------------------------------------------------------------------------

(in-package :cltcl)

;; ----------------------------------------------------------------------------
;; 0. The with-Tcl/Tk macro
;; ----------------------------------------------------------------------------

(defmacro WITH-TCL/TK ((VAR &REST ARGS) &BODY BODY)
  "Uses OPEN-TCL/TK-STREAM to start a Tcl/Tk interpreter and connect
to it by a two-way stream. Arguments ARGS are used as keyword
arguments to OPEN-TCL/TK-STREAM. The forms in BODY are evaluated as an
implicit progn with VAR bound to the stream returned by
OPEN-TCL/TK-STREAM. When control leaves the body, either normally or
abnormally, the stream is closed with CLOSE-TCL/TK-STREAM."
  `(let ((,var (open-Tcl/Tk-stream . ,args)))
     (unwind-protect (prog1 (progn ,@body)
		       (close-tcl/tk-stream ,var))
       (when (open-stream-p ,var)
	 (send-script ,var #TCL[exit])
	 (close-tcl/tk-stream ,var)))))

;; ----------------------------------------------------------------------------
;; 1. Connecting to Tcl/Tk
;; ----------------------------------------------------------------------------

(defun OPEN-TCL/TK-STREAM (&KEY INTERPRETER OPTIONS)
  "Starts a Tcl/Tk interpreter and creates and returns a two-way
stream connected to this Tcl/Tk process. This function and the meaning
of arguments INTERPRETER and OPTIONS are implementation dependent."
  (handler-case
      #+:CLISP (let ();;(custom:*default-file-encoding* :iso-8859-1))
		 (ext:run-program interpreter
				  :arguments options
				  :input     :stream
				  :output    :stream))
      #+:LUCID (lcl:run-program interpreter
		 :arguments options
		 :input     :stream
		 :output    :stream
		 :wait      NIL)
      #+:ALLEGRO (excl:run-shell-command
		     (format NIL "exec ~A~{ \"~A\"~}" 
		      interpreter options)
		   :input        :stream
		   :output       :stream
		   :wait         NIL)
      #+:sbcl (let ((process (sb-ext:run-program interpreter options
						 :input :stream
						 :output :stream
						 :wait nil)))
		(make-two-way-stream (sb-ext:process-output process)
				     (sb-ext:process-input process)))
      #+:abcl (let ((lispobject-class (java:jclass "org.armedbear.lisp.LispObject"))
		    (symbol-class (java:jclass "org.armedbear.lisp.Symbol"))
		    (inputstream-class (java:jclass "java.io.InputStream"))
		    (outputstream-class (java:jclass "java.io.OutputStream"))
		    (process-class (java:jclass "java.lang.Process")))
		(let ((SYSTEM_STREAM (java:jfield symbol-class "SYSTEM_STREAM"))
		      (CHARACTER (java:jfield symbol-class "CHARACTER")))
		  (let ((process (let ((Runtime (java:jclass "java.lang.Runtime")))
				   (java:jcall (java:jmethod Runtime "exec"
							     (java:jclass "java.lang.String"))
					       (java:jcall (java:jmethod Runtime "getRuntime")
							   Runtime)
					       interpreter))))
		    (let ((input-stream (java:jnew (java:jconstructor 
						    "org.armedbear.lisp.Stream" 
						    symbol-class
						    inputstream-class
						    lispobject-class)
						   SYSTEM_STREAM
						   (java:jcall (java:jmethod process-class
									     "getInputStream")
							       process)
						   CHARACTER))
			  (output-stream (java:jnew (java:jconstructor 
						     "org.armedbear.lisp.Stream" 
						     symbol-class 
						     outputstream-class
						     lispobject-class)
						    SYSTEM_STREAM
						    (java:jcall (java:jmethod process-class
									      "getOutputStream") 
								process)
						    CHARACTER)))
		      (make-two-way-stream input-stream output-stream)))))
      #+:cmu (let ((process (ext:run-program interpreter options
					     :input :stream
					     :output :stream
					     :wait nil)))
	       (make-two-way-stream (ext:process-output process)
				    (ext:process-input process)))
      #+:ccl (let ((process (ccl:run-program interpreter options
					     :input :stream
					     :output :stream
                                             :external-format :utf-8 ;;:iso-8859-1 ;; :L1
					     :wait nil)))
	       (make-two-way-stream 
		(ccl:external-process-output-stream process)
		(ccl:external-process-input-stream process)))
      #+:LispWorks (system:open-pipe 
		       (format NIL "~A~{ \"~A\"~}" 
			interpreter options))
      (condition (err)
	(error "~A~2%~A ~A~2%~A~%~A~{ ~A~}"
	       "Failed to open a stream to the Tcl/Tk interpreter."
	       "Reason: " (princ-to-string err)
	       "The following command was given:"
	       interpreter options))))

(defun CLOSE-TCL/TK-STREAM (STREAM)
  "Instructs the Tcl/Tk process associated with STREAM to exit and
closes the stream. Arguments STREAM must be a stream created by
OPEN-TCL/TK-STREAM. This function is implementation dependent."
  (close stream))

;; ----------------------------------------------------------------------------
;; 2. Communicating with Tcl/Tk
;; ----------------------------------------------------------------------------

(defun SEND-SCRIPT (STREAM SCRIPT)
  "Sends clTcl script SCRIPT to a running Tcl/Tk interpreter via
stream STREAM. The stream must be one that was opened with
OPEN-TCL/TK-STREAM. The script must be a list of strings, each string
being a valid Tcl command."
  (format stream "~{~A~%~}" script)
  (force-output stream))

(defun RECEIVE-LINE (STREAM)
  "Reads a Tcl list from STREAM including the terminating
character. Returns nil if an end of file occurs."
  (prog1 (read-list stream)
    (read-line stream nil nil t)))
