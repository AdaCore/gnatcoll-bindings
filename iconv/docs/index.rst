GNATcoll Bindings - Iconv: Converting between character encodings
=================================================================

.. index:: iconv
.. index:: charset
.. highlight:: ada

This package provides a binding to the libiconv library. This library is
standard on most Unix systems. When it is not provided by the system, the GNU
libiconv package can be installed instead.

Using GNATCOLL.Iconv
====================

Use the ``gnatcoll_iconv`` project in your project files. For instance::

     with "gnatcoll_iconv";
     project Default is
          ...
     end Default;

API
===

The whole API is documented in :file:`gnatcoll-iconv.ads`. Here is a simple
code sample that converts from iso-8859-1 encoding to UTF8::

    with GNATCOLL.Iconv;   use GNATCOLL.Iconv;
    procedure Main is
       EAcute : constant Character := Character'Val (16#E9#);
       --  in iso-8859-1

       Result : constant String := Iconv
          ("Some string " & EAcute,
           To_Code => UTF8,
           From_Code => ISO_8859_1);
    begin
       null;
    end Main;

A more advanced (and somewhat more efficient) API is available via the
``Iconv`` procedure. In that procedure, you control the input and output
buffers, so you will need less overall memory when you are converting big
buffers.
