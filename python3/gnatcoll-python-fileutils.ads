------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Bindings to functions declared in python include file fileutils.h

with GNATCOLL.Python.Ctypes;

package GNATCOLL.Python.Fileutils is

   package C renames GNATCOLL.Python.CTypes;

   function Py_DecodeLocale (Arg : String) return C.WChar_Addr;
   --  Decode a byte string from the locale encoding with the surrogateescape
   --  error handler: undecodable bytes are decoded as characters in range
   --  U+DC80..U+DCFF. If a byte sequence can be decoded as a surrogate
   --  character, escape the bytes using the surrogateescape error handler
   --  instead of decoding them.

   --  Encoding, highest priority to lowest priority:
   --    * UTF-8 on macOS, Android, and VxWorks;
   --    * UTF-8 on Windows if Py_LegacyWindowsFSEncodingFlag is zero;
   --    * UTF-8 if the Python UTF-8 mode is enabled;
   --    * ASCII if the LC_CTYPE locale is "C", nl_langinfo(CODESET) returns
   --      the ASCII encoding (or an alias), and mbstowcs() and wcstombs()
   --      functions uses the ISO-8859-1 encoding.
   --    * the current locale encoding.

   --  Return a pointer to a newly allocated wide character string, use
   --  PyMem_RawFree() to free the memory.

   --  Raise DecodingError or MemoryError in case of error

   --  Decoding errors should never happen, unless there is a bug in the C
   --  library.

end GNATCOLL.Python.Fileutils;
