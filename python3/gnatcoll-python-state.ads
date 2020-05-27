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

package GNATCOLL.Python.State is

   type PyGILState_STATE is private;

   PyGILState_LOCKED : constant PyGILState_STATE;
   PyGILState_UNLOCKED : constant PyGILState_STATE;

   function PyGILState_Ensure return PyGILState_STATE;
   pragma Import (C, PyGILState_Ensure, "PyGILState_Ensure");
   --  Ensure that the current thread is ready to call the Python C API
   --  regardless of the current state of Python, or of the global
   --  interpreter lock. This may be called as many times as desired by a
   --  thread as long as each call is matched with a call to
   --  PyGILState_Release().

   procedure PyGILState_Release (State : PyGILState_STATE);
   pragma Import (C, PyGILState_Release, "PyGILState_Release");
   --  Release any resources previously acquired. After this call, Python's
   --  state will be the same as it was prior to the corresponding
   --  PyGILState_Ensure().

private

   type PyGILState_STATE is new Integer;

   PyGILState_LOCKED : constant PyGILState_STATE := 0;
   PyGILState_UNLOCKED : constant PyGILState_STATE := 1;

end GNATCOLL.Python.State;
