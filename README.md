The GNAT Components Collection (GNATcoll) - Bindings
====================================================

This is the bindings module of the GNAT Components Collection. Please refer to
individual components for more details.

Dependencies
------------

This module depends on the following external components, that should be
available on your system:

- GPRbuild
- gnatcoll-core
- As well as relevant third-party libraries you need to build bindings for.

Building
--------

The components of GNATcoll Bindings are built using standalone GPR project
files. To build each of them you can simply do:

```sh
$ gprbuild -P <component>/gnatcoll-<component>.gpr
```

However, this method has several limitations:

* it builds one version of the library (static, relocatable and static-pic)
  at a time
* it might depend on the environment (`C_INCLUDE_PATH`, `LIBRARY_PATH`, ...)

In order to simplify that process, each component contains a Python script
called `setup.py`. Each script provides the following subcommands: `build`,
`install`, `clean`, `uninstall`.

On the first call to `build`, the user can setup some preferences. You can do
`setup.py build --help` to get the list of available options for each module.
After first call previous preferences will be reused unless you use the
`--reconfigure` switch.

Note that you can perform an out-of-source-tree build by just invoking
`setup.py` from another directory.


Installing
----------

In order to install a given component, either call `gprinstall` or use
`setup.py` script:

```sh
$ setup.py install --prefix=some_path
```

Note that if `--prefix` is not used, then projects will be installed into the
location of the used compiler.


Bindings
--------

The following bindings are provided:

- [gmp](gmp/README.md)
- [iconv](iconv/README.md)
- lzma
- [omp](omp/README.md)
- [python](python/README.md)
- [python3](python3/README.md)
- [readline](readline/README.md)
- [syslog](syslog/README.md)


Bug reports
-----------

Please send questions and bug reports to report@adacore.com following
the same procedures used to submit reports with the GNAT toolset itself.
