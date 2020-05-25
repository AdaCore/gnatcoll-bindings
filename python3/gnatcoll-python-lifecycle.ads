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

--  Bindings to functions controlling the interpreter lifecycle

with GNATCOLL.Python.CTypes;

package GNATCOLL.Python.Lifecycle is

   package C renames GNATCOLL.Python.CTypes;

   procedure Py_SetPythonHome (Home : C.WChar_Addr);
   pragma Import (C, Py_SetPythonHome, "Py_SetPythonHome");
   --  Set the default "home" directory, that is, the location of the standard
   --  Python libraries. See PYTHONHOME for the meaning of the argument string.

   --  The argument should point to a zero-terminated character string in
   --  static storage whose contents will not change for the duration of the
   --  program's execution. No code in the Python interpreter will change the
   --  contents of this storage.

   --  Use Py_DecodeLocale() to decode a bytes string to get a WChar_Addr
   --  string.

   procedure Py_SetPythonHome (Home : String);
   --  Same as previous function except that Py_DecodeLocale is called
   --  automatically.

   procedure Py_SetProgramName (Name : C.WChar_Addr);
   pragma Import (C, Py_SetProgramName, "Py_SetProgramName");
   --  This function should be called before Py_Initialize() is called for the
   --  first time, if it is called at all. It tells the interpreter the value
   --  of the argv[0] argument to the main() function of the program
   --  (converted to wide characters). This is used by Py_GetPath() and some
   --  other functions below to find the Python run-time libraries relative to
   --  the interpreter executable. The default value is 'python'. The argument
   --  should point to a zero-terminated wide character string in static
   --  storage whose contents will not change for the duration of the
   --  program's execution. No code in the Python interpreter will change the
   --  contents of this storage.

   --  Use Py_DecodeLocale() to decode a bytes string to get a WChar_Addr
   --  string.

   procedure Py_SetProgramName (Name : String);
   --  Same as previous procedure except that Py_DecodeLocale is called
   --  automatically

   procedure Py_SetProgramName;
   --  Same as previous procedure except that the procedure use
   --  Ada.Commmand_Line.Command_Name as Name.

   procedure Py_Initialize (Initialize_Signal_Handlers : Boolean := True);
   --  Initialize the Python interpreter. In an application embedding Python,
   --  this should be called before using any other Python/C API functions.
   --  For the few exceptions see Python documentation.

   --  This initializes the table of loaded modules (sys.modules), and creates
   --  the fundamental modules builtins, __main__ and sys. It also initializes
   --  the module search path (sys.path). It does not set sys.argv; use
   --  PySys_SetArgvEx() for that. This is a no-op when called for a second
   --  time (without calling Py_FinalizeEx() first). There is no return value;
   --  it is a fatal error if the initialization fails.

   --  If Initialize_Signal_Handlers is set to True (default) the
   --  initialization performs the registration of the signal handlers.
   --  If not, it skipsregistration of signal handlers, which might be useful
   --  when Python is embedded.

   procedure Py_Finalize;
   pragma Import (C, Py_Finalize, "Py_Finalize");
   --  Undo all initializations made by Py_Initialize() and subsequent use of
   --  Python/C API functions, and destroy all sub-interpreters (see
   --  Py_NewInterpreter() below) that were created and not yet destroyed since
   --  the last call to Py_Initialize(). Ideally, this frees all memory
   --  allocated by the Python interpreter. This is a no-op when called for a
   --  second time (without calling Py_Initialize() again first).

   function Py_Finalize return Boolean;
   --  Same as previous function but return True if finalization managed to
   --  free all the memory buffers. Return False otherwise.

   type Interpreter_Status is private;

   Interpreter_Exit_Normally : constant Interpreter_Status;
   Interpreter_Raise_Exception : constant Interpreter_Status;
   Interpreter_Invalid_Command_Line : constant Interpreter_Status;

   function Py_Main return Interpreter_Status;
   pragma Import (C, Py_Main, "__gnatcoll_py_main");
   --  The main program for the standard interpreter. This is made available
   --  for programs which embed Python. The return value will be
   --  Interpreter_Exit_Normally if the interpreter exits normally (i.e.,
   --  without an exception), Interpreter_Raise_Exception if the
   --  interpreter exits due to an exception, or
   --  Interpreter_Invalid_Command_Line if the parameter list does not
   --  represent a valid Python command line.

   --  Note that if an otherwise unhandled SystemExit is raised, this function
   --  will not return Interpreter_Raise_Exception, but exit the process, as
   --  long as Py_InspectFlag is not set.

private

   type Interpreter_Status is new Integer;

   Interpreter_Exit_Normally : constant Interpreter_Status := 0;
   Interpreter_Raise_Exception : constant Interpreter_Status := 1;
   Interpreter_Invalid_Command_Line : constant Interpreter_Status := 2;

end GNATCOLL.Python.Lifecycle;
