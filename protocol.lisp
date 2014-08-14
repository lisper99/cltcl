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

;; ----------------------------------------------------------------------------
;; clTcl communication protocol
;;
;; 0. Exported symbols
;; 1. Initialization of clTcl
;; 2. Helper functions
;; 3. Messages
;; 4. Tests
;;
;; Paul Griffioen 2009-2014
;; ----------------------------------------------------------------------------

(in-package :cltcl)

;; ----------------------------------------------------------------------------
;; 0. Exported symbols
;; ----------------------------------------------------------------------------

(defvar *INTERPRETER* "/usr/bin/wish"
  "The default Tcl/Tk interpreter. Default value is /usr/bin/wish.")

(defvar *STREAM* NIL
  "A two-way-stream connected to a Tcl/Tk interpreter or NIL if no
  such interpreter is running. This stream is used by functions CALL,
  POST and RUN.")

(defvar *TRACE-LEVEL* -1
  "An integer that determines which events are traced in clTcl's
communication protocol between Lisp and Tcl/Tk. Setting a larger
number gives trace messages. Level -1 (the default) is complete
silence, level 0 is errors only, level 1 is errors and events, level 2
is errors, events and event details.")

(defvar *DEBUG* NIL
  "If not nil the commands of a script are send one by one to the
  interpreter. This eases localization of errors. Default nil.")

(defun EVENT-LOOP (SCRIPT &KEY (INTERPRETER *INTERPRETER*) OPTIONS ARGUMENTS)
  "Starts Tcl/Tk interpreter located at INTERPRETER, binds *STREAM* to
the stream that connects to Tcl/Tk, binds *INTERPRETER* to
INTERPRETER, runs SCRIPT on ARGUMENTS and starts listening on the
stream. Use command 'exit' to end the listener. Establishes a binding
for restart KEEP-LISTENING to recover from errors in an event
handler. The default value for INTERPRETER is *INTERPRETER. Keyword
OPTIONS is passed to the interpreter."
  (let ((*interpreter* interpreter))
    (with-tcl/tk (*stream* :interpreter interpreter 
			   :options options)
      (loop
	 initially 
	   (handshake *stream*)
	   (log-status 1 "Setting up Tcl/Tk side...")
	   (run-script (Tcl/Tk-setup-script) *stream*)
	   (log-status 1 "Tcl/Tk side ready")
	   (log-status 1 "Running initial script")
	   (log-status 3 "~{~A~%~}" script)
	   (send-script *stream* 
			(script-for-command "set" (list "argv" arguments)))
	   (if *debug*
	       (dolist (x (clean-script script))
		 (log-status 1 "   -> Running command ~A" x)
		 (run-script (list x) *stream*))
	       (run-script script *stream*))
	   (log-status 1 "Initial script done")
	 for message = (progn (log-status 2 "Event loop listening...")
			      (receive-message *stream*))
	 do 
	   (log-status 1 "<- Incoming ~(~A~)" (message-tag message))
	   (log-status 2 "   with data ~S" (message-data message))
	 while message do
	   (if (eql (message-tag message) :event)
	       (progn (send-message *stream* "reply"
				    (handle-event (message-data message)))
		      (log-status 1 "-> Reply"))
	       (cerror "Keep listening for events."
		       "Invalid clTcl message: ~A" message))
	 finally
	   (log-status 1 "Event loop stopped")))))

(defun CALL (COMMAND &REST ARGS)
  "Calls Tcl/Tk command COMMAND with arguments ARGS properly
escaped. Arguments of type symbol are writting to a string and
prefixed with a hyphen to support keyword for Tcl options. Other
arguments are written to string if necessary and properly
escaped. This function is typically used in a event that was invoked
in a clTcl script run by EVENT-LOOP. Sends the command to *STREAM*."
  (log-status 1 "   -> Calling command ~A" command)
  (log-status 2 "      with arguments (~{~A~^ ~})" args)
  (run-sub-script (script-for-command command args) *stream*))

(defun POST (COMMAND &REST ARGS)
  "Posts Tcl/Tk command COMMAND with arguments ARGS properly escaped
as an event and does not wait for a reply. Arguments of type symbol
are writting to a string and prefixed with a hyphen to support keyword
for Tcl options. Other arguments are written to string if necessary
and properly escaped. Sends the command to *STREAM*."
  (log-status 1 "   -> Posting command ~A" command)
  (log-status 2 "      with arguments (~{~A~^ ~})" args)
  (send-script *stream* (script-for-command command args)))

(defun RUN (SCRIPT &REST ARGS)
  "Sends SCRIPT via *STREAM* to Tcl/Tk and waits for a reply. This
function is typically used in a event that was invoked in a clTcl
script run by EVENT-LOOP. Sets Tcl/Tk variable argv to ARGS, properly
escaped."
  (let ((commands (clean-script script)))
    (log-status 1 "   -> Running script of ~A commands" (length commands))
    (log-status 2 "      with arguments (~{~A~^ ~})" args)
    (log-status 3 "~{~A~%~}" commands)
    (run-sub-script (script-for-command "set" (list "argv" args)) *stream*)
    (if *debug*
        (loop
           for line in commands
           for result = (progn
                          (log-status 1 "   -> Running command ~A" line)
                          (run-sub-script (list line) *stream*))
           finally (return result))
        (run-sub-script commands *stream*))))

(defun KEEP-LISTENING ()
  "A restart applicable during errors in events. Transfers control
back to Tcl and resumes listening."
  (invoke-restart 'keep-listening))

;; ----------------------------------------------------------------------------
;; 1. Initialization of clTcl
;; ----------------------------------------------------------------------------

(defun HANDSHAKE (STREAM)
  "Sends a message to Tcl/Tk stream to test if it responds. Throws an
error when no appropriate message comes back."
  (log-status 1 "Handshaking...~%-> Sending handshake script")
  (let ((reply (progn
		 (send-script stream
		       #TCL[
		       puts "Tcl/Tk at your service. Version [info tclversion]"
		       flush stdout])
		 (receive-line stream))))
    (if (< 20 (mismatch reply "Tcl/Tk at your service."))
	(log-status 1 "<- Reply: ~A~%Handshake okay" reply)
	(progn 
	  (log-status 0 "Handshake failed, throwing an error.")
	  (error "Handshake with Tcl/Tk failed. Interpreter was ~A"
		 *interpreter*)))))

(defun TCL/TK-SETUP-SCRIPT ()
  "Script that creates Tcl procedures in Tcl/Tk to communicate to Lisp."
  #TCL[

  namespace eval ::cltcl:: {

      namespace export callLisp

      # Counterpart of receive-message on the Lisp side
      proc sendMessage {channel tag message} {
          puts $channel [list $tag $message]
	  flush $channel
      }
  
      # Counterpart of send-message on the Lisp side
      proc receiveMessage {channel} {
          set tag [gets $channel]
	  if {$tag == "exit"} {exit}
	  while { $tag != "script" && $tag != "reply" } {
	      after idle $tag
            set tag [gets $channel]
	      if {$tag == "exit"} {exit}
	  }
	  set message ""
	  set header [gets $channel]
	  while { $header != "DONE" } {
            set line [gets $channel]
	    append message $line\n
	    set header [gets $channel]
	  }
	  return [list $tag $message]
      }
  
      proc callLisp {fun args} {
          if [catch { 
	      sendMessage stdout :event [concat [list $fun] $args]
	      set message [receiveMessage stdin]
	      set tag [lindex $message 0]
	      while {$tag == "script"} {
	          set command [lindex $message 1]
		  if [catch {set tmp [namespace inscope :: $command]} err] {
		      sendMessage stdout :error $err
		  } else {
		      sendMessage stdout :reply $tmp
		  }
		  set message [ receiveMessage stdin]
		  set tag [lindex $message 0]
	      }
	      set result [lindex $message 1]
	  } error] {
	      tk_messageBox -message "Fatal error: $error"
	      exit
	  } else {
	      return $result
	  }
      }
  }])

;; ----------------------------------------------------------------------------
;; 2. Helper functions
;; ----------------------------------------------------------------------------

(defun SCRIPT-FOR-COMMAND (COMMAND ARGUMENTS)
  "Script that performs command with all arguments safely escaped."
  (format-script
   #TCL[~A ~A]
   command
   (write-list-to-tcl-string 
    (mapcar (lambda (x)
	      (typecase x
		(symbol (if (eql (find-package :keyword)
				 (symbol-package x))
			    (format nil "-~(~A~)" x)
			    x))
		(t x)))
	    arguments))))

(defun RUN-SCRIPT (SCRIPT &OPTIONAL (STREAM *STREAM*))
  "Sends a script directly to the Tcl/Tk interpreter and waits for a
reply."
  (send-script stream 
	       (format-script
		#TCL[
		if [catch {
		  if [catch {set reply [~{~A~%~}]} error] {
		    ::cltcl::sendMessage stdout :error $error
		  } else  {
		    ::cltcl::sendMessage stdout :reply $reply
		  }
		} error] {
		  tk_messageBox -message "Fatal error: $error"
		  exit
		}] script))
  (listen-for-reply stream))

(defun RUN-SUB-SCRIPT (SCRIPT &OPTIONAL (STREAM *STREAM*))
  "Sends a script to the listener that triggered the event that
triggered this call and waits for a reply. The script is picked up and
evaluated by a listener at the Tcl/Tk side."
  (send-message stream "script" (format nil "~{~A~%~}" script))
  (listen-for-reply stream))

(defun LISTEN-FOR-REPLY (STREAM)
  "Receives messages from STREAM until a reply is received. Incoming
events and errors are handled. Returns the data from the reply."
  (loop
     for message = (progn (log-status 2 "Listening for reply...")
			  (receive-message stream))
     for tag = (message-tag message)
     for data = (message-data message)
     do
       (log-status 1 "   <- Incoming ~(~A~)" tag)
       (log-status 2 "      with data ~S" data)
     until (or (null message) (eql tag :reply))
     do (case tag
	 (:event (send-message stream "reply" (handle-event data))
		 (log-status 1 "   -> Reply"))
	 (:error (error data))
	 (t (cerror "Keep listening for reply"
		    "Invalid tag while listening for reply: ~A" tag)))
     finally (return (when message data))))

(defun HANDLE-EVENT (EVENT)
  "Handles EVENT that was read from a Tcl/Tk stream. Calls the
requested Lisp function and returns the results. Restart
KEEP-LISTENING is available during the event."
  (let ((event-data (read-tcl-list-from-string1 event)))
    (let ((fun (let ((*read-eval*))
		 (read-from-string (first event-data))))
	  (args (rest event-data)))
      (log-status 1 "   Calling event handler ~A" fun)
      (log-status 2 "   with arguments (~{~A~^ ~})" args)
      (let ((result (with-simple-restart 
			(keep-listening
			 (format nil "Abort ~A" fun))
		      (let ((value (apply fun args)))
			(log-status 1 "   Event returned")
			(log-status 2 "   value ~S" value)
			value))))
	(typecase result
	  (list (write-list-to-tcl-string result))
	  (t (princ-to-string result)))))))

(defun LOG-STATUS (LEVEL CONTROL-STRING &REST ARGUMENTS)
  "Writes formatted text to *TRACE-OUTPUT* if LEVEL is at least as
high as *TRACE-LEVEL*. Level 0 is for errors, level 1 is for events,
level 2 is for event details."
  (when (<= level *trace-level*)
     (fresh-line *trace-output*)
     (apply #'format *trace-output* control-string arguments)
     (fresh-line *trace-output*)))

;; ----------------------------------------------------------------------------
;; 3. Messages
;; ----------------------------------------------------------------------------

(defun SEND-MESSAGE (STREAM TAG MESSAGE)
  "Counterpart is Tcl proc receiveMessage"
  (unless stream
    (error "Trying to use empty clTcl stream."))
  (format stream "~A~%" tag)
  (with-input-from-string (in message)
    (loop for line = (read-line in nil nil)
       while line do
	 (format stream "NEXT~%")
	 (format stream "~A~%" line)))
  (format stream "DONE~%")
  (force-output stream))

(defun RECEIVE-MESSAGE (STREAM)
  "Attempts to read a message from stream. Functions MESSAGE-TAG
yields the tag of the message. Function MESSAGE-DATA yields the
data. Counterpart of Tcl proc sendMessage"
  (unless stream
    (error "Trying to use empty clTcl stream."))
  (let ((message (receive-line stream)))
    (when message
      (let ((list (read-tcl-list-from-string1 message)))
	(when list
	  (list (let ((*read-eval*))
		  (read-from-string (first list)))
		(second list)))))))

(defun MESSAGE-TAG (MESSAGE)
  "The tag of the message obtained with RECEIVE-MESSAGE. One of the
symbols :reply, :event or :error."
  (first message))

(defun MESSAGE-DATA (MESSAGE)
  "The data of the message obtained with RECEIVE-MESSAGE. The contents
depends on the type of message."
  (second message))

;; ----------------------------------------------------------------------------
;; 4. Tests
;; ----------------------------------------------------------------------------

(defun TEST (&REST ARGS &KEY (TRACE-LEVEL 0) &ALLOW-OTHER-KEYS)
  "Tests the connection with Tcl/Tk and displays systems
information. Sets *TRACE-LEVEL* to TRACE-LEVEL. The other keywords are
passed to the event handler. See OPEN-TCL/TK-STREAM for allowed
keywords."
  (let ((script #TCL[

	  package require Tk
	  namespace import ::cltcl::callLisp
	  wm protocol . WM_DELETE_WINDOW exit

	  proc displayinfo {} {

	      lappend info \
	          "Machine:" "[callLisp machine-type] \
                             ([callLisp machine-version])" \
	          "System:" "[callLisp software-type]\
                            ([callLisp software-version])" \
	          "Common Lisp:" "[callLisp lisp-implementation-type]\
                                 ([callLisp lisp-implementation-version])" \
	          "Windowing:" "[tk windowingsystem]" \
 	          "Tcl:" "[info tclversion] ([info patchlevel])" \
	          "Executable:" "[info nameofexecutable]" \
	          "Library:" "[callLisp cltcl::funA]" \
		  "Encoding:" "[encoding system]"

	      set i 0
	      foreach {x y} $info {
	          label .tag$i -text $x
		  message .info$i -text $y -width 500
		  grid .tag$i .info$i -sticky nw
		  incr i
	      }

	      button .exitButton \
	          -width 10 \
		  -text "Close" \
	          -command {exit}
	      grid .exitButton -columnspan 2 -pady 4
	  }

	  proc funB {fun} {
	    return [callLisp $fun]
	  }

	  displayinfo

	  ])
	(*trace-level* trace-level))
    (apply #'cltcl:event-loop script :allow-other-keys t args)))

(defun funA ()
  "Helper for TEST"
  (cltcl:call "funB" "cltcl::funC"))

(defun funC ()
  "Helper for TEST"
  (cltcl:call "info" "library"))


(defun RUN-REPL (&REST ARGS &KEY (TRACE-LEVEL 0) &ALLOW-OTHER-KEYS)
  "Stub to test function REPL. Demonstrates how the repl might be
added to an application via an event-handler."
  (let ((*trace-level* trace-level))
    (apply #'cltcl:event-loop #TCL[	  package require Tk
	  namespace import ::cltcl::callLisp
	  wm protocol . WM_DELETE_WINDOW {tk_messageBox -message "Type 'exit' in the repl"}
	  callLisp cltcl::repl]
	  :allow-other-keys t args)))

(defun REPL ()
  "A read-eval-print loop. Useful for development. Prompts for
commands to send to the Tcl/Tk interpreter and prints the reply."
  (loop 
     initially 
       (call "update" "idletasks")
       (fresh-line)
       (format t "Welcome in the clTcl repl~%Type 'exit' to quit.~%")
     for command = (progn (fresh-line)
			  (princ "% ")
			  (receive-line *standard-input*))
     until (or (equal command "exit") 
	       (null command)
	       (null (handler-case 
			 (loop
			    initially (send-message *stream* "script" command)
			    for line = (receive-line *stream*)
			    for items = (when line
					  (read-tcl-list-from-string1 line))
			    for possible-tag = (first items)
			    until (or (null line)
				      (equal possible-tag ":reply")
				      (equal possible-tag ":error"))
			    do (princ line)
			    finally 
			      (when line (call "update" "idletasks"))
			      (return (princ (second items))))
		       (condition (error)
			 (princ error)))))
     finally 
       (when (equal command "exit")
	 (send-message *stream* "script" "exit"))
       (princ "bye")
       (fresh-line)))
