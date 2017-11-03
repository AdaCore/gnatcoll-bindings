The GNAT Components Collection (GNATCOLL) - Readline
====================================================

This is the Readline component of the GNAT Components Collection.

This component provides an interface to the readline library.
This library provides support for interactive input from the user,
providing nice key bindings to edit the current line (including support
for backspace, move to beginning or end of line,...), as well as support
for completion (via the <tab> key) and history (via up and down keys).

Readline is licensed under the Full GNU General Public License. If you
distribute a program using this package and the readline library, this
program must be free software. 

When building, you need to pass an explicit option `--accept-gpl`
to indicate that you accept and understand the terms of the license.

Dependencies
------------

This component requires the following external components, that should be
available on your system:

- gprbuild
- gnatcoll-core
- readline
