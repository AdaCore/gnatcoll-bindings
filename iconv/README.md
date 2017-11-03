The GNAT Components Collection (GNATCOLL) - Iconv
=================================================

This is the Iconv component of the GNAT Components Collection.

It is an interface to libiconv.
There are multiple variants of libiconv: on some Unix systems it is part
of the C library, whereas other systems have installed the GNU libiconv
separately. Those variants work slightly differently.

For historical reasons, international text is often encoded using a
language or country dependent character encoding. With the advent of the
internet and the frequent exchange of text across countries - even the
viewing of a web page from a foreign country is a "text exchange" in this
context -, conversions between these encodings have become important. They
have also become a problem, because many characters which are present in
one encoding are absent in many other encodings. To solve this mess, the
Unicode encoding has been created. It is a super-encoding of all others and
is therefore the default encoding for new text formats like XML.

However, many computers still operate in locale with a traditional (limited)
character encoding. Some programs, like mailers and web browsers, must be
able to convert between a given text encoding and the user's encoding.
Other programs internally store strings in Unicode, to facilitate internal
processing, and need to convert between internal string representation
(Unicode) and external string representation (a traditional encoding) when
they are doing I/O. Libiconv is a conversion library for both kinds of
applications.

Dependencies
------------

This component requires the following external components, that should be
available on your system:

- gprbuild
- gnatcoll-core
- iconv
