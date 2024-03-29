------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

--  An Ada interface to the GNU Multiple Precision (GMP) arithmetic library.
--  See child packages for specific types, such as package GMP.Integers.

with Interfaces.C;

package GNATCOLL.GMP is

   pragma Pure;

   --  We define these numeric types here so that clients of the Ada binding
   --  do not also have to import package Interfaces.C themselves.
   --  These types correspond to those used by the underlying C implementation
   --  of the GMP library itself.

   type Int is new Interfaces.C.int;

   type Long is new Interfaces.C.long;

   type Unsigned_Long is new Interfaces.C.unsigned_long;

   type Double is new Interfaces.C.double;

end GNATCOLL.GMP;
