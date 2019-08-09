The GNAT Components Collection (GNATCOLL) - Python
==================================================

This is the Python component of the GNAT Components Collection.

Standard interface to the Python 2 interpreter. NOTE: This binding is not
compatible with Python 3.

Dependencies
------------

This component requires the following external components, that should be
available on your system:

- gprbuild
- gnatcoll-core
- Python 2, at least version 2.3, but the most recent available version of
  Python 2 from ww.python.org is recommended.

NOTE for Windows users: if you are installing the official distrib, you should
install it in "just for me" mode, otherwise the python DLL will be placed in
C:\Windows\System32 folder and it will result in shared library's link failure.
The workaround in this case is to copy it by hand back to python install dir.
