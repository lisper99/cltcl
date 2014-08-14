---
title: clTcl
layout: default
---


Introduction
------------

Tcl/Tk provides a powerful GUI library with a BSD-style open source
license. It is a combination of the scripting language Tcl and the
cross-platform widget toolkit Tk.

Package clTcl provides an interface to Tcl/Tk via Tcl scripts embedded
into Common Lisp. Program your GUI with Tcl as embedded
language. Scripts can be run by choosing one of the various Tcl/Tk
interpreters. Deployment is possible for many systems.


Hello World
-----------

The following fragment is the obligatory Hello World.

    (cltcl:event-loop
      #TCL[package require Tk
           wm protocol . WM_DELETE_WINDOW {exit}
           label .message -text "Hello World!"
           pack .message])

Function event-loop starts the Tcl/Tk interpreter, executes the given
script and starts listening for events to dispatch. It is typically
used as application top loop.

The Tcl script is embedded with syntax `#TCL[ ... ]`. At the dots any
literal Tcl script can occur.


Documentation
-------------

The [installation page](installation.html) describes the requirements,
installing clTcl, choosing a Tcl/Tk interpreter, troubleshooting.

The [manual](manual.html) explains how to use the clTcl package. All
functions are [specification](specification.html) of the clTcl package.

Finally there are some [examples](examples.html).


Status
------

I have run it successfully with ABCL, CLISP, SBCL, LispWorks, CCL and
CMU on GNU/Linux and with ABCL, CCL, CLISP and LispWorks on
Windows. All combinations worked for various Tcl/Tk
interpreters. Other implementations and combinations should work
without many problems.


Links
-----

- An excellent Tcl tutorial is [Tcl for Web Nerds][webnerds] by
  Abelson, Greenspun and Sandon.
- Many Tcl/Tk resources can be found at the [Tcl Developer
  Xchange][xchange] and the [Tclers Wiki][wiki].
- Documentation for all Tcl and Tk commands can be found on the
  Developer Xchange manual pages. See for example the manuals pages
  for [Tcl/Tk 8.4][tcltk84], [Tcl/Tk 8.5][tcltk85] and [Tcl/Tk
  8.6][tcltk86].

[webnerds]: http://philip.greenspun.com/tcl/
[xchange]: http://tcl.tk/
[wiki]: http://wiki.tcl.tk/
[tcltk84]: http://www.tcl.tk/man/tcl8.4/contents.htm
[tcltk85]: http://www.tcl.tk/man/tcl8.5/contents.htm
[tcltk86]: http://www.tcl.tk/man/tcl8.6/contents.htm

License
-------

This software has an MIT license.