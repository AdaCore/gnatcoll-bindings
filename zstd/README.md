The GNAT Components Collection (GNATCOLL) - ZSTD
================================================

This is the ZSTD component of the GNAT Components Collection.

It is an interface to the ZSTD Compression/Decompression library

Dependencies
------------

This component requires the following external components, that should be
available on your system:

- python 3.x: needed to launch the configuration script
- e3-testsuite python package (optional): to launch the testsuite
- gprbuild: builder for the project
- gnatcoll-core library: dependency
- zstd library >= 1.5.0: dependency


Project Structure
-----------------

- config/gnatcoll_zstd_constants.gpr: contains default values for scenario
  variables that are computed by the configuration script. 
- gnatcoll_zstd.gpr: project to be withed in order to use the library
- gnatcoll_zstd.gpr.py: configuration script
- README.md: the present file
- src/: library sources
    - src/gnatcoll-zstd.ads: low-level binding to ZSTD C API
    - src/gnatcoll-zstd-streams.ads: low-level binding to ZSTD stream C API
    - src/gnatcoll-zstd-controlled.ads: higher level binding that use
      controlled objects for context and raise Ada exceptions in case of error.
    - src/gnatcoll-coders-zstd.ads: Ada stream support using GNATCOLL.Coders
- testsuite/: testsuite
    - run-tests


Building
--------

The simplest way to build and install the project is to run the following
command:

    $ cd <BUILD_DIR>
    $ <SOURCE_DIR>/gnatcoll_zstd.gpr.py build --prefix=<INSTALL_DIR> --install

The command will build both static and shared version of the library and install
it in **INSTALL_DIR**

The Python script provides more options. For each command help can displayed by
doing:

    $ gnatcoll_zstd.gpr.py COMMAND --help
