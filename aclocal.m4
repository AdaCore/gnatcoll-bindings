## This file contains various m4 macros that can be included in your
## own projects to enable proper detection of the scripting languages
## In your own aclocal.m4 file, you can use syntax like
##   include(gnatcoll/aclocal.m4)

##############################################################
# Machine-specific linker switches
#   @EXTRA_LINK_SWITCHES@: list of system-specific linker
#      switches. Its syntax is compatible with GPR files.
##############################################################

AC_DEFUN(AM_SYSTEM_LINK_SWITCHES,
[
    case $build_os in
       *darwin*)   EXTRA_LINK_SWITCHES='"-Wl,-no_pie"';;
       *)          EXTRA_LINK_SWITCHES='';;
    esac
    AC_SUBST(EXTRA_LINK_SWITCHES)
])

##############################################################
# Checking for build type
# The following variable is exported by configure:
#   @BUILD_TYPE@: either "Production" or "Debug"
##############################################################

AC_DEFUN(CHECK_BUILD_TYPE,
[
  AC_ARG_ENABLE(build,
    [AC_HELP_STRING(
       [--enable-build=<type>],
       [Default build type for the library (Debug, Production)])],
    BUILD_TYPE=$enableval,
    BUILD_TYPE=Production)
  AC_SUBST(BUILD_TYPE)
])

#############################################################
# Check whether gnatmake can compile, bind and link an Ada program
#    AM_TRY_ADA(gnatmake,filename,content,success,failure)
#############################################################

AC_DEFUN(AM_TRY_ADA,
[
   cat > conftest.ada <<EOF
[$3]
EOF
   if AC_TRY_COMMAND([gnatchop -q conftest.ada && $1 $2 >/dev/null 2>conftest.out])
   then
      : Success
      $4
   else
      : Failure
      $5
   fi
   rm -rf conftest.ada
])

#############################################################
# Check whether platform/GNAT supports atomic increment/decrement
# operations.
# The following variable is then set:
#     SYNC_COUNTERS_IMPL
# to either "intrinsic" or "mutex"
# Code comes from the PolyORB configure.ac
#############################################################

AC_DEFUN(AM_HAS_INTRINSIC_SYNC_COUNTERS,
[
  AC_MSG_CHECKING([whether platform supports atomic inc/dec])
  AM_TRY_ADA([gnatmake], [check.adb],
[
with Interfaces; use Interfaces;
procedure Check is
   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32;
   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   X : aliased Interfaces.Integer_32;
   Y : Interfaces.Integer_32 := 0;
   pragma Volatile (Y);
   --  On some platforms (e.g. i386), GCC has limited support for
   --  __sync_add_and_fetch_4 for the case where the result is not used.
   --  Here we want to test for general availability, so make Y volatile to
   --  prevent the store operation from being discarded.
begin
   Y := Sync_Add_And_Fetch (X'Access, 1);
end Check;
],
[
   AC_MSG_RESULT(yes)
   SYNC_COUNTERS_IMPL="intrinsic"
],[
   AC_MSG_RESULT(no)
   SYNC_COUNTERS_IMPL="mutex"
])

   rm -f check.adb check
   AC_SUBST(SYNC_COUNTERS_IMPL)
])

#############################################################
# Check whether we have the GNAT sources available
# The following variables are exported by configure:
#   @WITH_PROJECTS@: "yes" or "no"
#############################################################

AC_DEFUN(AM_GNAT_SOURCES,
[
  AC_MSG_CHECKING(whether libgpr exists)
  AM_HAS_GNAT_PROJECT(gpr)
  if test "$HAVE_GNAT_PROJECT_gpr" = "yes"; then
     HAS_GNAT_SOURCES=yes
  else
     HAS_GNAT_SOURCES=no
  fi
])

AC_DEFUN(AM_PROJECTS,
[
  # Allow the user to disable projects support so that gnatcoll does not depend
  # on the installed gnat compiler (libgprs project)
  AC_ARG_ENABLE(projects,
    AC_HELP_STRING(
      [--disable-projects],
      [Disable support for GNAT Projects [[default=enabled]]]),
    [WITH_PROJECTS=$enableval],
    [WITH_PROJECTS=$HAS_GNAT_SOURCES])

  AC_SUBST(WITH_PROJECTS)
])

#############################################################
# Check whether GNAT on that target supports building shared
# libraries
# The following variables are exported by configure:
#   @GNAT_BUILDS_SHARED@: either "yes" or "no"
#   @DEFAULT_LIBRARY_TYPE@: either "static" or "relocatable"
#############################################################

AC_DEFUN(AM_GNAT_BUILDS_SHARED,
[
   AC_MSG_CHECKING(whether gnat can build shared libs)

   DEFAULT_LIBRARY_TYPE=static

   AC_ARG_ENABLE(shared,
     [AC_HELP_STRING(
        [--disable-shared],
        [Disable building of shared libraries])
AC_HELP_STRING(
        [--enable-shared],
        [Build shared libraries if supported on the target
Make them the installation default])],
     [GNAT_BUILDS_SHARED=$enableval
      if test $enableval = yes; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [GNAT_BUILDS_SHARED=yes])

   if test x$GNAT_BUILDS_SHARED = xyes; then
      mkdir conftest conftest/lib
      echo "package Foo is end Foo;" > conftest/foo.ads
      cat > conftest/lib.gpr <<EOF
project Lib is
   for Source_Dirs use (".");
   for Library_Dir use "lib";
   for Library_Name use "lib";
   for Library_Kind use "relocatable";
end Lib;
EOF

      if AC_TRY_COMMAND([gprbuild -c -q -Pconftest/lib]); then
         GNAT_BUILDS_SHARED=yes
      else
         GNAT_BUILDS_SHARED=no
         DEFAULT_LIBRARY_TYPE=static
      fi
      rm -rf conftest
      AC_MSG_RESULT($GNAT_BUILDS_SHARED)
   else
      AC_MSG_RESULT([no (--disabled-shared)])
   fi

   AC_SUBST(GNAT_BUILDS_SHARED)
   AC_SUBST(DEFAULT_LIBRARY_TYPE)
])

#############################################################
# Checking for syslog
# This checks whether syslog exists on this system.
# This module can be disabled with
#    --disable-syslog
# The following variables are exported by configure:
#    @WITH_SYSLOG@: either "yes" or "no"
############################################################

AC_DEFUN(AM_PATH_SYSLOG,
[
   AC_ARG_ENABLE(syslog,
     AC_HELP_STRING(
        [--disable-syslog],
        [Disable support for syslog [[default=enabled]]]),
     [WITH_SYSLOG=$enableval],
     [WITH_SYSLOG=yes])

   if test x$WITH_SYSLOG = xyes ; then
     AC_CHECK_HEADER([syslog.h],
                     [WITH_SYSLOG=yes],
                     [WITH_SYSLOG=no])
   fi

   AC_SUBST(WITH_SYSLOG)
])

#############################################################
# Checking for iconv.
# There are multiple versions of the library (Solaris, GNU,...)
# and some of them have slightly different APIs.
# Iconv on AIX is currently not supported because it does not
# support passing an empty string to iconv_open to specify the
# locale charset.
# The following variables are exported by configure:
#   @WITH_ICONV@: either "yes", "no" or "static"
#   @PATH_ICONV@: -Lpath/to/libiconv, or "" if not found
#   @INCLUDE_ICONV@: the "-I..." for iconv.h, if needed
#   @LIB_ICONV@:  either "" or "-liconv"
#############################################################

AC_DEFUN(AM_PATH_ICONV,
[
   NEED_ICONV=no
   INCLUDE_ICONV=""
   LIB_ICONV=""
   PATH_ICONV=""
   WITH_ICONV=yes
   ICONV_STATIC=no

   AC_ARG_WITH(iconv,
     [AC_HELP_STRING(
       [--with-iconv=<path>],
       [Specify the full path to the iconv library])
AC_HELP_STRING(
       [--without-iconv],
       [Disable iconv support])],
     [ICONV_PATH_WITH=$withval; NEED_ICONV=yes],
     [ICONV_PATH_WITH=yes])

   AC_MSG_CHECKING(for libiconv)
   case $ICONV_PATH_WITH in
       no)
          # Explicitly disabled by user (--with-iconv=no or --without-iconv)
          AC_MSG_RESULT([no, disabled by user])
          WITH_ICONV=no
          ;;

       static)
          WITH_ICONV=no
          ICONV_STATIC=yes
          LD_LIBRARY_PATH="/opt/local/lib:/usr/local/lib:/usr/lib:/lib:$LD_LIBRARY_PATH"
          as_save_IFS=$IFS; IFS=$PATH_SEPARATOR
          for dir in $LD_LIBRARY_PATH ; do
              if test -f "$dir/libiconv.a"; then
                 am_path_iconv=$dir
                 WITH_ICONV=yes
                 PATH_ICONV=""
                 LIB_ICONV="$dir/libiconv.a"
                 LIBS="$LIBS $LIB_ICONV"
                 INCLUDE_ICONV="-I$dir/../include"
                 break
              fi
          done
          IFS=$as_save_IFS
          ;;

       yes)
          # Request automatic detection
          #
          # A special case for instance on Solaris: iconv.h is installed in
          # /usr/include, but (on our machines at least) also in
          # /usr/local/include. Both variants are different, and we must make
          # sure we link with the matching library. If /usr/local/lib is not
          # in LD_LIBRARY_PATH, AM_LIB_PATH will not find libiconv (part of
          # the standard library), so will not add -liconv. But the default
          # search path for cpp is such that /usr/local/include is searched
          # first (even if -I/usr/include is on the command line, since it is
          # also a default search path). So we have a discrepency. So we
          # always add the default search paths to LD_LIBRARY_PATH.

          case $target in
              *aix*)
                 WITH_ICONV=no
                 AC_MSG_RESULT([no, unsupported on AIX])
                 ;;
              *darwin*)
                 # On OSX, we do not want to use macport's version of libiconv,
                 # which is not compatible with the system (we end up with
                 # errors like _iconv_open not found for architecture x86_64).
                 # So we force the use of /usr/lib. Note that in that case we
                 # don't want to add a -L/usr/lib. Otherwise all libraries
                 # present in that directory (libpython, ...) will be taken
                 # first at that location and so break other configure
                 # switches such --with-python.

                 LIB_ICONV="/usr/lib/libiconv.dylib"
                 INCLUDE_ICONV="-I/usr/include"
                 PATH_ICONV=""
                 ;;
              *)
                LD_LIBRARY_PATH="/usr/local/lib:/usr/lib:/usr/target/lib:$LD_LIBRARY_PATH"

                AM_LIB_PATH(iconv)
                if test x"$am_path_iconv" != x ; then
                   PATH_ICONV="-L$am_path_iconv"
                   INCLUDE_ICONV="-I$am_path_iconv/../include"
                fi
                ;;
          esac
          ;;

       *)
          # path provided by user
          am_path_iconv="$ICONV_PATH_WITH"
          INCLUDE_ICONV="-I$am_path_iconv/include"

          # If the user has disabled shared libs, we will build the tools
          # with static libs. In this case, we want to force static libiconv
          # too, even if --with-iconv=path was used.

          if test x"$GNAT_BUILDS_SHARED" = xno ; then
             AC_MSG_RESULT([(static because of --disable-shared)])
             PATH_ICONV=""
             LIB_ICONV="$am_path_iconv/lib/libiconv.a"
             ICONV_STATIC=yes
          else
             PATH_ICONV="-L$am_path_iconv/lib"
          fi

          ;;
   esac

   if test x"$WITH_ICONV" = x"yes" ; then
      _save_LIBS="$LIBS"
      if test x"$PATH_ICONV" = x ; then
          AC_MSG_RESULT(no special -L needed)
      else
          AC_MSG_RESULT(found in $am_path_iconv)
          LIBS="$LIBS $PATH_ICONV"
      fi

      CFLAGS="$CFLAGS $INCLUDE_ICONV"

      # The code below is similar to AC_SEARCH_LIBS(iconv_open, [iconv])
      # but allows us to also check that #include <iconv.h> is supported
      # and matches the library we link with. Otherwise, we might be
      # compiling with /usr/local/include/iconv.h (default search path)
      # but linking without -liconv (since on Solaris it is part of the
      # libc and that would be detected).

      AC_LANG_CONFTEST(
         [AC_LANG_PROGRAM(
            [#include <iconv.h>],
            [iconv_open(0,0)])])

      if test "$ICONV_STATIC" = "no" -a "$LIB_ICONV" = ""; then
         AC_MSG_CHECKING([for library containing iconv_open])
         for ac_lib in '' iconv ; do
             if test -z "$ac_lib" ; then
                switch=""
             else
                switch="-l$ac_lib"
             fi
             LIBS="$switch $LIBS"
             AC_LINK_IFELSE([],
                [WITH_ICONV=yes;
                 LIB_ICONV="$switch";
                 break],
                [WITH_ICONV=no])
         done
         AC_MSG_RESULT([$WITH_ICONV $LIB_ICONV])
      fi
      rm -f conftest.$ac_objext conftest$ac_exeext
   
      LIBS="$_save_LIBS"     # Should we leave -liconv ?

      if test x"$WITH_ICONV" = xno -a x"$NEED_ICONV" = xyes ; then
        AC_MSG_ERROR([iconv not found])
      fi
   fi

   AC_SUBST(WITH_ICONV)
   AC_SUBST(PATH_ICONV)
   AC_SUBST(LIB_ICONV)
   AC_SUBST(INCLUDE_ICONV)
])

#############################################################
# Search for a library anywhere on LD_LIBRARY_PATH
# This will return the empty string if not found, and the directory
# otherwise.
# This can be used to add -Ldir parameters on the command line: if
# the library was found with AC_CHECK_LIB but not this macro, this
# means it is in the standard search path and no command line switch
# is required.
#    AM_LIB_PATH(libname)
# Sets am_path_<libname> to the path
#############################################################

AC_DEFUN(AM_LIB_PATH,
[
   am_path_$1=""
   as_save_IFS=$IFS; IFS=$PATH_SEPARATOR
   for lib_dir in $LD_LIBRARY_PATH /usr /usr/local /opt/local
   do
      IFS=$as_save_IFS
      _AS_ECHO_LOG([Testing $lib_dir/lib$1${SO_EXT}])
      if test -f "$lib_dir/lib$1${SO_EXT}"; then
         am_path_$1=$lib_dir
         break
      fi
   done
   IFS=$as_save_IFS
])

#############################################################
# Checking for postgreSQL
# This checks whether the libpq exists on this system
# This module can be disabled with
#    --with-postgresql=path
# The following variables are exported by configure:
#   @WITH_POSTGRES@: whether postgres was detected
#   @PATH_LIBPQ@: path to libpq, or "" if not found
#   @HAS_PQPREPARE@: whether the version of postgreSQL has PQprepare ()
#############################################################

AC_DEFUN(AM_PATH_POSTGRES,
[
   NEED_PSQL=no
   AC_ARG_WITH(postgresql,
     [AC_HELP_STRING(
       [--with-postgresql=<path>],
       [Specify the full path to the PostgreSQL installation])
AC_HELP_STRING(
       [--without-postgresql],
       [Disable PostgreSQL support])],
     [POSTGRESQL_PATH_WITH=$withval; NEED_PSQL=yes],
     POSTGRESQL_PATH_WITH=yes)

   PATH_LIBPQ=""
   if test x"$POSTGRESQL_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for PostgreSQL)
      AC_MSG_RESULT(no, use --with-postgresql if needed)
      WITH_POSTGRES=no

   else
     if test x"$POSTGRESQL_PATH_WITH" = xyes ; then
       PATH_LIBPQ=`pg_config 2>/dev/null | grep ^LIBDIR | cut -d\  -f3`
       if test x"$PATH_LIBPQ" != x ; then
          PATH_LIBPQ="-L$PATH_LIBPQ"
          LIB_PQ="-lpq"
       else
          AM_LIB_PATH(pq)
          if test x"$am_path_pq" != x ; then
             PATH_LIBPQ="-L$am_path_pq"
             LIB_PQ="-lpq"
          fi
       fi
       AC_CHECK_LIB(pq,PQreset,WITH_POSTGRES=yes,WITH_POSTGRES=no,[$PATH_LIBPQ])

     else
       WITH_POSTGRES=yes

       # Did the user provide a path to a static libpq ?

       if test -f "$POSTGRESQL_PATH_WITH" -a `basename "$POSTGRESQL_PATH_WITH"` = "libpq.a" ; then
          PATH_LIBPQ=""
          LIB_PQ="$POSTGRESQL_PATH_WITH -lssl -lcrypto"

       elif test -f "$POSTGRESQL_PATH_WITH/libpq.so" -o -f "$POSTGRESQL_PATH_WITH/libpq.dll" -o -f "$POSTGRESQL_PATH_WITH/libpq.dylib" ; then
          PATH_LIBPQ="-L$POSTGRESQL_PATH_WITH"
          LIB_PQ="-lpq"
       elif test -f "$POSTGRESQL_PATH_WITH/lib/libpq.so" -o -f "$POSTGRESQL_PATH_WITH/lib/libpq.dll" -o -f "$POSTGRESQL_PATH_WITH/lib/libpq.dylib" ; then
          PATH_LIBPQ="-L$POSTGRESQL_PATH_WITH/lib"
          LIB_PQ="-lpq"
       else
          AC_MSG_CHECKING(for PostgreSQL)
          AC_MSG_RESULT(not found in $POSTGRESQL_PATH_WITH)
          WITH_POSTGRES=no
       fi
     fi

     if test x"$WITH_POSTGRES" = xno -a x"$NEED_PSQL" = xyes ; then
       AC_MSG_ERROR([PostgreSQL not found])
     fi
   fi

   if test x"$WITH_POSTGRES" = xyes ; then
     AC_CHECK_LIB(pq,PQprepare,HAS_PQPREPARE=yes,HAS_PQPREPARE=no,[$PATH_LIBPQ])
   else
     HAS_PQPREPARE=no
   fi

   AC_SUBST(WITH_POSTGRES)
   AC_SUBST(PATH_LIBPQ)
   AC_SUBST(LIB_PQ)
   AC_SUBST(HAS_PQPREPARE)
])

j############################################################
# Checking for sqlite
# This checks whether sqlite is installed on the system. It can
# be disabled with
#    -with-sqlite=no
# The following variables are exported by configure:
#    @WITH_SQLITE@: whether sqlite is detected
#    @PATH_LIBSQLITE@: path to libsqlite3
#############################################################

AC_DEFUN(AM_PATH_SQLITE,
[
   NEED_SQLITE=no
   AC_ARG_WITH(sqlite,
     [AC_HELP_STRING(
        [--with-sqlite=<path>],
        [Specify the full path to the sqlite installation, or "embedded" (default)])
AC_HELP_STRING(
        [--without-sqlite],
        [Disable sqlite support])],
     [SQLITE_PATH_WITH=$withval; NEED_SQLITE=yes],
     SQLITE_PATH_WITH=yes)

   PATH_LIBSQLITE=""
   if test x"$SQLITE_PATH_WITH" = xembedded -o x"$SQLITE_PATH_WITH" = xyes ; then
      AC_MSG_CHECKING(for sqlite)
      AC_MSG_RESULT(embedded, use --with-sqlite to use a dynamic lib)
      WITH_SQLITE=embedded

   else
      if test x"$SQLITE_PATH_WITH" = xno ; then
        AC_MSG_CHECKING(for sqlite)
        AC_MSG_RESULT(no, use --with-sqlite to use a dynamic lib)
        WITH_SQLITE=no
      else
         if test x"$SQLITE_PATH_WITH" != xyes ; then
           if test -d $SQLITE_PATH_WITH/lib; then
              PATH_LIBSQLITE="-L$SQLITE_PATH_WITH/lib"
           elif test -d $SQLITE_PATH_WITH/lib64; then
              PATH_LIBSQLITE="-L$SQLITE_PATH_WITH/lib64"
           else
              PATH_LIBSQLITE="-L$SQLITE_PATH_WITH"
           fi
         fi

         # Requires at least version 3.7.14
         AC_CHECK_LIB(sqlite3, sqlite3_close_v2,
                      [WITH_SQLITE=yes],
                      [WITH_SQLITE=no],
                      $SQLITE_CFLAGS $PATH_LIBSQLITE)

         if test x"$WITH_SQLITE" = xno ; then
            AC_MSG_CHECKING(for sqlite)
            AC_MSG_RESULT(embedded, use --with-sqlite to use a dynamic lib)
            WITH_SQLITE=embedded
         fi
      fi
   fi

   case "${host}" in
      *solaris2.10 )
          SQLITE_CFLAGS='"-std=c99"'
          ;;
      *)
          SQLITE_CFLAGS=''
          ;;
   esac

   AC_SUBST(WITH_SQLITE)
   AC_SUBST(PATH_LIBSQLITE)
   AC_SUBST(SQLITE_CFLAGS)

])

#############################################################
# Checking for gmp
# This checks whether the gnu multiprecision library is available.
# The result can be forced by using the
#    --with-gmp=path
# The following variables are exported on exit:
#   @GMP_CFLAGS@:  Compiler flags
#   @GMP_LIBS@:    Extra command line switches for the linker
#   @WITH_GMP@:    "yes" or "no" depending on whether gmp is available
#############################################################

AC_DEFUN(AM_PATH_GMP,
[
   AC_ARG_WITH(gmp,
     [AC_HELP_STRING(
       [--with-gmp=<path>],
       [Specify the full path of the gmp install])
AC_HELP_STRING(
       [--without-gmp],
       [Disable support for gmp])],
     GMP_PATH_WITH=$withval,
     GMP_PATH_WITH=yes)

   GMP_CFLAGS=""
   GMP_LIBS=""
   if test x"$GMP_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for gmp)
      AC_MSG_RESULT(no, use --with-gmp if needed)
      WITH_GMP=no

   else
     if test x"$GMP_PATH_WITH" = xyes ; then
       AC_CHECK_LIB(gmp,__gmpz_init,WITH_GMP=yes,WITH_GMP=no)
       AC_CHECK_HEADER(gmp.h, [], [WITH_GMP=no])
       GMP_LIBS="-lgmp"
     else
       GMP_LIBS="-L$GMP_PATH_WITH/lib -lgmp"
       GMP_CFLAGS="-I$GMP_PATH_WITH/include"
       WITH_GMP=yes
     fi
   fi

   AC_SUBST(WITH_GMP)
   AC_SUBST(GMP_CFLAGS)
   AC_SUBST(GMP_LIBS)
])

#############################################################
# Checking for readline
#############################################################

AC_DEFUN(AM_CHECK_READLINE,
[
   AC_ARG_ENABLE(readline,
     [AC_HELP_STRING(
        [--disable-readline],
        [Disable support for readline])],
     WITH_READLINE=$enableval,
     WITH_READLINE="")

   if test "$WITH_GPL" = "no" ; then
      AC_MSG_CHECKING(for readline)
      AC_MSG_RESULT([no, this is a pure GPL library (see --enable-gpl)])
      WITH_READLINE=no
   elif test "$WITH_READLINE" = "" ; then
      AC_CHECK_LIB(readline,readline,WITH_READLINE=yes,WITH_READLINE=no)
   elif test "$WITH_READLINE" = "yes" ; then
      AC_CHECK_LIB(readline,readline,WITH_READLINE=yes,WITH_READLINE=no)
      if test "$WITH_READLINE" = "no" ; then
         AC_MSG_ERROR([Readline not found])
      fi
   fi

   AC_SUBST(WITH_READLINE)
])

#############################################################
# Checking for mmap
# The following variables are exported:
#   @WITH_MMAP@: either "yes" or "no"
#############################################################

AC_DEFUN(AM_MMAP,
[
   AC_FUNC_MMAP

   if test $ac_cv_func_mmap_fixed_mapped = yes; then
      WITH_MMAP=yes
   else
      WITH_MMAP=no
   fi
   AC_SUBST(WITH_MMAP)
])

#############################################################
# Checking for python
# This checks whether python is available on the system, and if yes
# what the paths are. The result can be forced by using the
#    --with-python=path
# command line switch
# The following variables are exported by configure on exit:
#    @PYTHON_BASE@:    Either "no" or the directory that contains python
#    @PYTHON_VERSION@: Version of python detected
#    @PYTHON_CFLAGS@:  Compiler flags to use for python code
#    @PYTHON_DIR@:     Directory for libpython.so
#    @PYTHON_LIBS@:    extra command line switches to pass to the linker.
#    @WITH_PYTHON@: either "yes" or "no" depending on whether
#                      python support is available.
#############################################################

AC_DEFUN(AM_PATH_PYTHON,
[
   NEED_PYTHON=no

   AC_ARG_WITH(python,
     [AC_HELP_STRING(
       [--with-python=<path>],
       [Specify the prefix of the Python installation])
AC_HELP_STRING(
       [--without-python],
       [Disable python support])],
     [PYTHON_PATH_WITH=$withval; NEED_PYTHON=$PYTHON_PATH_WITH],
     PYTHON_PATH_WITH=yes)
   AC_ARG_WITH(python-exec,
     [AC_HELP_STRING(
        [--with-python-exec=<path>],
        [forces a specific python executable (python3 for instance)])],
     [PYTHON_EXEC=$withval])
   AC_ARG_ENABLE(shared-python,
     AC_HELP_STRING(
       [--enable-shared-python],
       [Link with shared python library instead of static]),
     PYTHON_SHARED=$enableval,
     PYTHON_SHARED=no)

   if test "$PYTHON_EXEC" = ""; then
      PYTHON_EXEC="python"
   fi

   WITH_PYTHON=yes
   if test x"$PYTHON_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for python)
      AC_MSG_RESULT(no, use --with-python if needed)
      PYTHON_BASE=no
      WITH_PYTHON=no
   elif test "$PYTHON_PATH_WITH" = "yes"; then
      AC_PATH_PROG(PYTHON, ${PYTHON_EXEC}, no)
      if test "$PYTHON" = "no"; then
         PYTHON_BASE=no
         WITH_PYTHON=no
      fi
   else
      AC_MSG_CHECKING(for python)
      if "$PYTHON_PATH_WITH/bin/${PYTHON_EXEC}" --version >/dev/null 2>&1; then
         PYTHON="$PYTHON_PATH_WITH/bin/${PYTHON_EXEC}"
         AC_MSG_RESULT(yes)
      elif "$PYTHON_PATH_WITH/${PYTHON_EXEC}" --version >/dev/null 2>&1; then
         PYTHON="$PYTHON_PATH_WITH/${PYTHON_EXEC}"
         AC_MSG_RESULT(yes)
      else
         AC_MSG_RESULT(no, invalid python path)
         PYTHON_BASE=no
         WITH_PYTHON=no
      fi
   fi

   # Check that Python version is >= 2.0
   if test "$WITH_PYTHON" = "yes"; then
      AC_MSG_CHECKING(for python >= 2.0)
      python_major_version=`$PYTHON -c 'import sys; print(sys.version_info[[0]])' 2>/dev/null`
      python_version=`$PYTHON -c 'import sys; print(".".join([str(k) for k in sys.version_info]))' 2>/dev/null`
      if test "$python_major_version" -lt 2; then
         AC_MSG_RESULT(no, need at least version 2.0)
         PYTHON_BASE=no
         WITH_PYTHON=no
      else
         AC_MSG_RESULT(yes (version $python_version))
      fi
   fi

   # Find CFLAGS and LDFLAGS to link with Python
   if test "$WITH_PYTHON" = "yes"; then
      AC_MSG_CHECKING(if can link with Python library)
      result=`cat <<EOF | $PYTHON
from distutils.sysconfig import get_config_var, get_python_inc, get_config_vars
import sys
print 'PYTHON_VERSION=%s' % get_config_var("VERSION")
python_current_prefix=sys.prefix
config_args = [[k.replace("'", "") for k in get_config_vars().get('CONFIG_ARGS','').split("' '")]]
python_build_prefix=[[k.replace('--prefix=', '') for k in config_args if k.startswith('--prefix=')]]
if python_build_prefix:
    python_build_prefix = python_build_prefix[[0]]
else:
    python_build_prefix = sys.prefix
print 'PYTHON_BASE="%s"' % python_current_prefix
libpl = get_config_var('LIBPL')
if not libpl:
    libpl = '%s/libs' % python_current_prefix
else:
    if libpl.startswith(python_build_prefix) and not libpl.startswith(python_current_prefix):
        libpl = libpl.replace(python_build_prefix, python_current_prefix, 1)

libdir = get_config_var('LIBDIR')
if not libdir:
    libdir = python_current_prefix
else:
    if libdir.startswith(python_build_prefix) and not libdir.startswith(python_current_prefix):
        libdir = libdir.replace(python_build_prefix, python_current_prefix, 1)
print 'PYTHON_STATIC_DIR="%s"' % libpl
print 'PYTHON_SHARED_DIR="%s"' % libdir
cflags = " ".join(("-I" + get_python_inc().replace(python_build_prefix, python_current_prefix, 1),
                   "-I" + get_python_inc(plat_specific=True).replace(python_build_prefix, python_current_prefix, 1)))
print 'PYTHON_CFLAGS="%s"' % cflags
print 'PYTHON_LIBS="%s %s"' % (get_config_vars().get("LIBS", ""), get_config_vars().get("SYSLIBS", ""))
EOF
`
      eval "$result"
      if test "$PYTHON_SHARED" = "yes"; then
         PYTHON_DIR="$PYTHON_SHARED_DIR"
         PYTHON_LIBS="-L$PYTHON_DIR -lpython$PYTHON_VERSION $PYTHON_LIBS"
      else
         PYTHON_DIR="$PYTHON_STATIC_DIR"
         if test -f "${PYTHON_DIR}/libpython${PYTHON_VERSION}.a"; then
            PYTHON_LIBS="${PYTHON_DIR}/libpython${PYTHON_VERSION}.a $PYTHON_LIBS"
         else
            PYTHON_LIBS="-L$PYTHON_DIR -lpython$PYTHON_VERSION $PYTHON_LIBS"
         fi
      fi

      SAVE_CFLAGS="${CFLAGS}"
      SAVE_LIBS="${LIBS}"
      CFLAGS="${SAVE_CFLAGS} ${PYTHON_CFLAGS}"
      LIBS="${SAVE_LIBS} ${PYTHON_LIBS}"

      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([
/*    will only work with gcc, but needed to use it with the mingwin python */
#define PY_LONG_LONG long long
#include <Python.h>
],[   Py_Initialize();])],
        [AC_MSG_RESULT(yes)],
        [AC_MSG_RESULT(no)
         WITH_PYTHON=no
         PYTHON_BASE=no])

     # Restore an environment python-free, so that further tests are not
     # impacted in case we did not find python
     CFLAGS="${SAVE_CFLAGS}"
     LIBS="${SAVE_LIBS}"
   fi

   if test x"$WITH_PYTHON" = xno -a x"$NEED_PYTHON" != xno ; then
     AC_MSG_ERROR([Python not found])
   fi

   if test "$WITH_PYTHON" = "yes"; then
     AC_MSG_CHECKING(for python LDFLAGS)
     AC_MSG_RESULT($PYTHON_LIBS)
     AC_MSG_CHECKING(for python CFLAGS)
     AC_MSG_RESULT($PYTHON_CFLAGS)
   fi
   AC_SUBST(PYTHON_BASE)
   AC_SUBST(PYTHON_VERSION)
   AC_SUBST(PYTHON_DIR)
   AC_SUBST(PYTHON_LIBS)
   AC_SUBST(PYTHON_CFLAGS)
   AC_SUBST(WITH_PYTHON)
])

###########################################################################
## Checking for pygobject
##
###########################################################################

AC_DEFUN(AM_PATH_PYGOBJECT,
[
    AC_ARG_ENABLE(pygobject,
      AC_HELP_STRING(
        [--disable-pygobject],
        [Disable support for PyGobject [[default=enabled]]]),
      [WITH_PYGOBJECT=$enableval],
      [WITH_PYGOBJECT=$WITH_PYTHON])

    AC_MSG_CHECKING(for pygobject)

    if test "$PKG_CONFIG" = "" -o "$PKG_CONFIG" = "no" ; then
       AC_MSG_RESULT(no (pkg-config not found))
       WITH_PYGOBJECT=no

    elif test "$GTK_VERSION" = "no" ; then
       AC_MSG_RESULT(no (gtk+ not found))
       WITH_PYGOBJECT=no

    elif test x"$WITH_PYGOBJECT" = x -o x"$WITH_PYGOBJECT" = xno ; then
       AC_MSG_RESULT(no (disabled by user))
       WITH_PYGOBJECT=no

    else
       for version in 3.0 2.0 ; do
           module="pygobject-$version"
           $PKG_CONFIG $module --exists
           if test $? = 0 ; then
               break;
           fi
           module=""
       done

       if test "$module" = "" ; then
          AC_MSG_RESULT(no)
          WITH_PYGOBJECT=no
       else
          PYGOBJECT_INCLUDE=`$PKG_CONFIG $module --cflags`
          PYGOBJECT_LIB=`$PKG_CONFIG $module --libs`
          AC_MSG_RESULT(yes ($version))
          WITH_PYGOBJECT=yes
          PYGOBJECT_INCLUDE="$PYGOBJECT_INCLUDE -DPYGOBJECT"
       fi
    fi

    AC_SUBST(WITH_PYGOBJECT)
    AC_SUBST(PYGOBJECT_INCLUDE)
    AC_SUBST(PYGOBJECT_LIB)
])

###########################################################################
## Checking for pygtk
##   $1=minimum pygtk version required
## This function checks whether pygtk exists on the system, and has a recent
## enough version. It exports the following variables:
##    @WITH_PYGTK@:    "yes" or "no"
##    @PYGTK_PREFIX@:  installation directory of pygtk
##    @PYGTK_INCLUDE@: cflags to use when compiling a pygtk application
## This function must be called after the variable PKG_CONFIG has been set,
## ie probably after gtk+ itself has been detected. Python must also have been
## detected first.
###########################################################################


AC_DEFUN(AM_PATH_PYGTK,
[
    AC_ARG_ENABLE(pygtk,
      AC_HELP_STRING(
        [--disable-pygtk],
        [Disable support for PyGTK [[default=enabled]]]),
      [WITH_PYGTK=$enableval],
      [WITH_PYGTK=$WITH_PYTHON])

    if test "$PKG_CONFIG" = "" -o "$PKG_CONFIG" = "no" ; then
       AC_MSG_CHECKING(for pygtk)
       AC_MSG_RESULT(no (pkg-config not found))
       WITH_PYGTK=no

    elif test "$GTK_VERSION" != "2.0" ; then
       AC_MSG_CHECKING(for pygtk)
       AC_MSG_RESULT(no (incompatible gtk+ version))
       WITH_PYGTK=no

    else
       min_pygtk_version=ifelse([$1], ,2.8,$1)
       module=pygtk-2.0
       AC_MSG_CHECKING(for pygtk - version >= $min_pygtk_version)

       if test x"$WITH_PYGTK" = x -o x"$WITH_PYGTK" = xno ; then
          AC_MSG_RESULT(no)
          PYGTK_PREFIX=""
          PYGTK_INCLUDE=""
          WITH_PYGTK=no

       elif test "$PYTHON_BASE" != "no" ; then
          $PKG_CONFIG $module --exists
          if test $? != 0 ; then
             AC_MSG_RESULT(no)
             WITH_PYGTK=no

          else
             pygtk_version=`$PKG_CONFIG $module --modversion`
             $PKG_CONFIG $module --atleast-version=$min_pygtk_version
             if test $? = 0 ; then
                PYGTK_INCLUDE="`$PKG_CONFIG $module --cflags` -DPYGTK"
                PYGTK_PREFIX=`$PKG_CONFIG $module --variable=prefix`
                AC_MSG_RESULT(yes (version $pygtk_version))
                WITH_PYGTK=yes
             else
                AC_MSG_RESULT(no (found $pygtk_version))
                PYGTK_PREFIX=""
                PYGTK_INCLUDE=""
                WITH_PYGTK=no
             fi
          fi

       else
          AC_MSG_RESULT(no since python not found)
          PYGTK_PREFIX=""
          PYGTK_INCLUDE=""
          WITH_PYGTK=no
       fi
    fi

    AC_SUBST(PYGTK_PREFIX)
    AC_SUBST(PYGTK_INCLUDE)
    AC_SUBST(WITH_PYGTK)
])

##########################################################################
## Compute the extension for shared libraries
##########################################################################

AC_DEFUN(AM_SO_SUFFIX,
[
    case $build_os in
      *darwin*) SO_EXT=.dylib ;;
      *cygwin*|*mingw*)  SO_EXT=.dll ;;
      *)        SO_EXT=.so ;;
    esac
    AC_SUBST(SO_EXT)
])

##########################################################################
## Converts a list of space-separated words into a list suitable for
## inclusion in .gpr files
##   $1=the list
##   $2=exported name
##########################################################################

AC_DEFUN(AM_TO_GPR,
[
   value=[$1]

   # Special handling on darwin for gcc 4.5 and 4.7
   case "$build_os" in
      *darwin*)
         value=`echo $value | sed -e "s/-framework \([[^ ]]*\)/-Wl,-framework -Wl,\1/g"`
   esac

   output=$2
   result=""
   for v in $value; do
      if test "$result" != ""; then
         result="$result, "
      fi
      result="$result\"$v\""
   done
   $2=$result
   AC_SUBST($2)

])

##############################################################
# Usage: AM_HAS_GNAT_PROJECT(project)
# Check whether a given project file is available, and set
# HAVE_GNAT_PROJECT_<project> to "yes" or "no" accordingly.
# (from PolyORB ada.m4)
##############################################################

AC_DEFUN([AM_HAS_GNAT_PROJECT],
[
cat > conftest.gpr <<EOF
with "[$1]";
project Conftest is for Source_Files use (); end Conftest;
EOF
if AC_TRY_COMMAND([gprls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
then
  HAVE_GNAT_PROJECT_$1=yes
else
  # Perhaps we do not have "gprls" (for old gnat versions)
  # Try with gnatls
  if AC_TRY_COMMAND([gnat ls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
  then
    HAVE_GNAT_PROJECT_$1=yes
  else
    HAVE_GNAT_PROJECT_$1=no
  fi
fi
AC_MSG_RESULT($HAVE_GNAT_PROJECT_$1)
AC_SUBST(HAVE_GNAT_PROJECT_$1)
])

##########################################################################
## Detects GTK and GtkAda
## Input:
##   If CONFIGURE_SWITCH_WITH_GTK is set, it specifies the default value
##     for gtk. Otherwise, configure will choose the most recent version.
## This exports the following variables
##     @PKG_CONFIG@: path to pkg-config, or "no" if not found
##     @GTK_GCC_FLAGS@: cflags to pass to the compiler. It isn't call
##                      GTK_CFLAGS for compatibility reasons with GPS
##     @WITH_GTK@: Either "yes" or "no", depending on whether gtk+ was found
##     @GTK_VERSION@: one of 2.0, 3.0 or "no"
##########################################################################

AC_DEFUN(AM_PATH_GTK,
[
   AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
   if test "$PKG_CONFIG" = "no" ; then
      WITH_GTK=no
      GTK_VERSION=no
   else
      AC_ARG_WITH(gtk,
         AC_HELP_STRING(
       [--with-gtk=version],
       [Specify the version of GTK to support (3.0 or 2.0)])
AC_HELP_STRING(
       [--without-gtk],
       [Disable support for GTK]),
         [WITH_GTK=$withval],
         [
            AC_MSG_CHECKING(for default gtk+ version)
            # Detect the version we should use, from the system
            for WITH_GTK in "$CONFIGURE_SWITCH_WITH_GTK" "3.0" "2.0" "no"; do
                if test "$WITH_GTK" != ""; then
                   GTK_PREFIX=`$PKG_CONFIG gtk+-${WITH_GTK} --variable=prefix`
                   if test "$GTK_PREFIX" != ""; then
                      break
                   fi
                fi
            done
            AC_MSG_RESULT($WITH_GTK)
         ])

      if test "$WITH_GTK" != "no"; then
          AC_MSG_CHECKING(for gtk+ ${WITH_GTK})
          GTK_PREFIX=`$PKG_CONFIG gtk+-${WITH_GTK} --variable=prefix`
          AC_MSG_RESULT($GTK_PREFIX)
          GTK_GCC_FLAGS=`$PKG_CONFIG gtk+-${WITH_GTK} --cflags`
          GTK_GCC_LIBS=`$PKG_CONFIG gtk+-${WITH_GTK} --libs`
          if test x"$GTK_GCC_FLAGS" != x ; then
             AC_MSG_CHECKING(for gtkada.gpr)
             AM_HAS_GNAT_PROJECT(gtkada)
             HAVE_GTKADA=$HAVE_GNAT_PROJECT_gtkada
             GTK_VERSION=$WITH_GTK
             WITH_GTK=${HAVE_GTKADA}
          else
             GTK_VERSION=no
             WITH_GTK=no
          fi
      fi
   fi

   AC_SUBST(PKG_CONFIG)
   AC_SUBST(GTK_GCC_FLAGS)
   AC_SUBST(GTK_GCC_LIBS)
   AC_SUBST(WITH_GTK)
   AC_SUBST(GTK_VERSION)
])
