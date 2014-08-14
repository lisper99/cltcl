---
title: clTcl Examples
layout: default
---

Asteroids
---------

The [Asteroids](samples/asteroids.lisp) example
demonstrates how Tcl code is embedded in Common Lisp.

<div style="text-align: center;">
  <a href="images/asteroids.png">
    <img class="rfloat"
	 src="images/asteroids-small.png"
         alt="Screenshot of a Tcl/Tk remake of the classic Arcade game Asteroids."/>
  </a>
</div>


Common Lisp Package Viewer
--------------------------

The [Common Lisp Package Viewer](samples/package-viewer.lisp) shows
how to use the basic functionalily of clTcl.

<div style="text-align: center;">
  <a href="images/package-viewer.png">
    <img class="rfloat"
	 src="images/package-viewer-small.png" 
         alt="Dialog displaying information about ASDF symbol OPERATE."/>
  </a>
</div>


Fourier Series Demo
-------------------
	
The [Fourier series](samples/fourier.lisp) example demonstrates
graphics. It uses [PlotChart][plotchart] to display harmonics of the
Fourier series.

<div style="text-align: center;">
  <a href="images/fourier.png">
    <img class="rfloat"
         src="images/fourier-small.png" 
         alt="Dialog displaying various harmonics of the Fourier Series of some function."/>
  </a>
</div>

You need PlotChart. The [binaries from the installation
page](installation.html#binaries) have PlotChart already in them.

[plotchart]: http://tcllib.sourceforge.net/doc/plotchart.html


ASDF Viewer
-----------

The [ASDF viewer](samples/asdf-viewer.lisp) is a more elaborate
example. Shows various ways to call Tcl/Tk or post events and handle
errors.

<div style="text-align: center;">
  <a href="images/asdf-viewer.png">
    <img class="rfloat"
	 src="images/asdf-viewer-small.png" 
         alt="Dialog displaying the available ASDF systems."/>
  </a>
</div>


Only works for SBCL and LispWorks but porting to other Lisps with
multiprocessing capabilities should not be difficult.
