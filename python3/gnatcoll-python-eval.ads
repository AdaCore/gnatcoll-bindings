------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2020, AdaCore                     --
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

package GNATCOLL.Python.Eval is

   function PyEval_SaveThread return PyThreadState;
   pragma Import (C, PyEval_SaveThread, "PyEval_SaveThread");
   --  Release the global interpreter lock (if it has been created) and reset
   --  the thread state to NULL, returning the previous thread state (which
   --  is not NULL). If the lock has been created, the current thread must
   --  have acquired it.

   procedure PyEval_RestoreThread (State : PyThreadState);
   pragma Import (C, PyEval_RestoreThread, "PyEval_RestoreThread");
   --  Acquire the global interpreter lock (if it has been created) and set
   --  the thread state to State, which must not be NULL. If the lock has
   --  been created, the current thread must not have acquired it, otherwise
   --  deadlock ensues.

end GNATCOLL.Python.Eval;
