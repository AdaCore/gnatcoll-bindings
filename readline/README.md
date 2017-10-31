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

When building, you need to pass an explicit variable ACCEPT_GPL=yes
to indicate that you accept and understand the terms of the license.

Dependencies
------------

This component requires the following external components, that should be
available on your system:

- gprbuild
- gnatcoll-core
- readline

Configuring the build process
-----------------------------

The following variables can be used to configure the build process:

General:

   prefix     : location of the installation, the default is the running
                GNAT installation root.

   BUILD      : control the build options : PROD (default) or DEBUG

   PROCESSORS : parallel compilation (default is 0, which uses all available
                cores)

   TARGET     : for cross-compilation, auto-detected for native platforms

   SOURCE_DIR : for out-of-tree build

   INTEGRATED : treat prefix as compiler installation (yes/no)
                this is so that installed gnatcoll project can later be
                referenced as predefined project of this compiler;
                this adds a normalized target subdir to prefix
                default is "no"

Module-specific:

   ACCEPT_GPL       : indicates your acceptance of GPL (yes/no)
                      default is "no"; required for building the component

To use the default options:

   $ make setup ACCEPT_GPL=yes

Building
--------

The component is built using a standalone GPR project file.

However, to build all versions of the library (static, relocatable and
static-pic) it is simpler to use the provided Makefile:

$ make

Then, to install it:

$ make install


Bug reports
-----------

Please send questions and bug reports to report@adacore.com following
the same procedures used to submit reports with the GNAT toolset itself.
