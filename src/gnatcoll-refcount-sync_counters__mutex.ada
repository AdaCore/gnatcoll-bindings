------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with GNAT.Task_Lock;

separate (GNATCOLL.Refcount)
package body Sync_Counters is

   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32
   is
      Result : Interfaces.Integer_32;
   begin
      GNAT.Task_Lock.Lock;
      Ptr.all := Ptr.all + Value;
      Result := Ptr.all;
      GNAT.Task_Lock.Unlock;
      return Result;
   end Sync_Add_And_Fetch;

end Sync_Counters;
