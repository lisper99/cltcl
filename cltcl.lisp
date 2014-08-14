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
;; clTcl
;;
;; 0. Exported functions
;; 1. Other functions
;;
;; Paul Griffioen 2009-2014
;;-----------------------------------------------------------------------------

(in-package :cltcl)

;; ----------------------------------------------------------------------------
;; 0. Exported functions
;; ----------------------------------------------------------------------------

(defun READ-SCRIPT (&OPTIONAL (STREAM *STANDARD-INPUT*) TERMINATOR)
  "Reads a Tcl script from STREAM. Reads Tcl commands until READ-LIST
finds TERMINATOR (when given) or an end of file occurs. Returns the
commands as a list of strings. The default value for STREAM is
*STANDARD-INPUT*."
  (loop 
     for command = (read-list stream terminator)
     for char = (read-char stream nil)
     when command collect command
     until (or (null char)
	       (eql char terminator))))

(defun READ-LIST (&OPTIONAL (STREAM *STANDARD-INPUT*) TERMINATOR)
  "Reads a Tcl list from STREAM. Reads Tcl words until an end of file
occurs or READ-WORD finds TERMINATOR (when given), a semi-colon or a
newline. The Tcl list is returned as a string. The default value for
STREAM is *STANDARD-INPUT*. Returns NIL if end of file is found
immediately."
  (read-white-space stream)
  (let ((char (peek-char nil stream nil)))
    (cond ((null char) nil)
	  ((eql char #\#) (read-comment stream))
	  (t (with-output-to-string (s)
	       (loop
		  for word = (read-word stream t t t terminator t)
		  while word do 
		    (write-string word s)
		    (write-string (read-white-space stream) s)))))))

(defun READ-WORD (STREAM &OPTIONAL
		   (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P TERMINATOR ECHO-P)
  "Reads a Tcl word from STREAM and returns it as a string. Reads
characters untill the next one is a terminator. The terminator itself
is not read. Terminators are TERMINATOR (when given), whitespace or
Tcl list terminators. Throws an error if an end of file occurs, unless
EOF-ERROR-P is nil, in which case it returns EOF-VALUE. If ECHO-P is
non nil then escape characters are not handled and the word is
literally copied."
  (read-white-space stream)
  (let ((char (peek-char nil stream eof-error-p recursive-p))) ;; is recursive-p okay?
    (cond ((null char) 
	   eof-value)
	  ((member char (list terminator #\; #\return #\newline))
	   nil)
	  ((eql char #\")
	   (read-double-quoted stream echo-p))
	  ((eql char #\{)
	   (read-braced stream echo-p))
	  (t (read-word-aux stream eof-error-p terminator echo-p)))))

(defun READ-TCL-LIST-FROM-STRING1 (STRING)
  "Converts Tcl list STRING into a list of strings. Items are
delimited by whitespace."
  (with-input-from-string (stream string)
    (loop 
       for word = (prog1 (read-word stream nil)
		    (read-white-space stream))
       while word collect word)))

(defun WRITE-LIST-TO-TCL-STRING (LIST)
  "Converts list to a Tcl string. Nested lists are converted
recursively. Strings are escaped. Other values are written to string
and escaped." 
  (with-output-to-string (s)
    (write-tcl-string list s)))

(defun WRITE-TCL-STRING (OBJECT &OPTIONAL (STREAM *STANDARD-OUTPUT*) RECURSIVEP)
  "Converts list to a Tcl string. Nested lists are converted
recursively. Strings are escaped. Other values are written to string
and escaped." 
  (typecase object
    (list (loop
	     initially (when recursivep
			 (write-char #\{ stream))
	     for (item . rest) on object
	     do (write-tcl-string item stream t)
	     while rest
	     do (write-char #\space stream)  
	     finally (when recursivep
		       (write-char #\} stream))))
    (string (if (and recursivep (equal object ""))
		(princ "{}" stream)
		(write-escaped object stream)))
    ;;(number (write (floor object) :stream stream))
    (float (format stream "~,10,F" object))
    (t (write-escaped (princ-to-string object) stream))))

(defun FORMAT-SCRIPT (SCRIPT &REST ARGS)
  "Applies function FORMAT to SCRIPT's commands sequentially. Each
element consumes its required formatter arguments from ARGS, leaving
the rest of the arguments for the rest of the commands. For clTcl
script written with #TCL[...] this gives the effect of a single format
on the entire Tcl code."
  (mapcar (lambda (x)
	    (with-output-to-string (s)
	      (setf args (apply (formatter "~@?") s x args))))
	  script))

(set-dispatch-macro-character
 #\# #\T 
 (lambda (stream char i)
   (declare (ignore char i))
   (assert (and (eql (read-char stream) #\C)
		(eql (read-char stream) #\L)
		(eql (read-char stream) #\[))
	   ()
	   "Invalid clTcl, use #TCL[...]")
   (list 'quote (read-script stream #\]))))

;; ----------------------------------------------------------------------------
;; 1. Other functions
;; ----------------------------------------------------------------------------

(defun READ-WORD-AUX (STREAM EOF-ERROR-P TERMINATOR ECHO-P)
  "Helper for READ-WORD."
  (with-output-to-string (s)
    (loop
       for next = (peek-char nil stream eof-error-p)
       until (or (null next)
		 (when terminator
		   (eql next terminator))
		 (member next '(#\space #\tab #\return #\newline #\;)))
       do (let ((char (read-char stream)))
	    (cond ((eql char #\[)
		   (format s "[~{~A~^~%~}]" (read-script stream #\])))
		  ((eql char #\\)
		   (let ((next-char (read-char stream)))
		     (if echo-p
			 (progn (write-char char s)
				(write-char next-char s))
			 (write-escaped-char next-char s))))
		  (t (write-char char s)))))))

(defun READ-WHITE-SPACE (STREAM)
  "Reads white space from STREAM, except newlines."
  (with-output-to-string (s)
    (loop
       for next = (peek-char nil stream nil)
       while (member next '(#\space #\tab))
       do (write-char (read-char stream) s))))

(defun READ-COMMENT (STREAM)
  "Reads a line of Tcl comment from STREAM but leaves the newline."
  (with-output-to-string (s)
    (loop for char = (peek-char nil stream)
	 until (member char (list #\return #\newline))
	 do (write-char (read-char stream) s))))

(defun READ-DOUBLE-QUOTED (STREAM &OPTIONAL ECHO-P)
  "Reads a Tcl double quoted expression from STREAM."
  (with-output-to-string (s)
    (loop initially
	 (let ((quote (read-char stream)))
	   (when echo-p (write-char quote s)))
	 for char = (read-char stream)
	 until (eql char #\")
	 do (if (eql char #\\)
		(let ((next-char (read-char stream)))
		  (if echo-p
		      (progn (write-char char s)
			     (write-char next-char s))
		      (write-escaped-char next-char s)))
		(write-char char s))
	 finally (when echo-p (write-char char s)))))

(defun READ-BRACED (STREAM &OPTIONAL ECHO-P)
  "Reads a Tcl braced expression from STREAM."
   (with-output-to-string (s)
     (loop 
	initially (let ((brace (read-char stream)))
		    (when echo-p (write-char brace s)))
	for next = (peek-char nil stream)
	until (eql next #\})
	do (cond ((eql next #\{)
		  (write-string (read-braced stream t) s))
		 ((eql next #\\)
		  (let ((next-char (read-char stream)))
		    (when (and (not echo-p)
			       (eql next-char #\newline))
		      'todo) ;checken dmv repl
		    (write-char next-char s))
		  (write-char (read-char stream) s))
		 (t (write-char (read-char stream) s)))
	finally (let ((brace (read-char stream)))
		  (when echo-p (write-char brace s))))))

(defun ESCAPE (X)
  "Escapes all characters that have a special meaning for
Tcl (includes whitespace) with a backslash."
  (with-output-to-string (stream)
    (write-escaped (if (stringp x) x (princ-to-string x))
		   stream)))

(defun WRITE-ESCAPED (X STREAM)
  "Escapes all characters that have a special meaning for
Tcl (includes whitespace) with a backslash."
  (with-input-from-string (s x)
    (loop for ch = (read-char s nil)
       while ch do
       (when (member ch '(#\newline #\return
			  #\space #\tab
			  #\\ #\$ #\; #\"
			  #\[ #\]
			  #\{ #\}))
	 (write-char #\\ stream))
       (write-char (case ch
		     (#\Backspace #\b)
		     (#\Page #\f)
		     (#\Newline #\n)
		     (#\Return #\r)
		     (#\Tab #\t)
		     (t ch))
		   stream))))

(defun WRITE-ESCAPED-CHAR (CHAR STREAM)
  "Writes character CHAR to stream. Special Tcl escape characters
\\b, \\f, \\r,\\n and \\t are converted."
  (write-char (case char
		(#\b #\Backspace)
		(#\f #\Page)
		(#\n #\Newline)
		(#\r #\Return)
		(#\t #\Tab)
		(t char))
	      stream))

(defun CLEAN-SCRIPT (SCRIPT)
  "Removes all empty strings and comments from command list SCRIPT."
  (loop
     for line in script
     unless (or (string-equal line "")
                (char= (aref line 0) #\#))
     collect line))
