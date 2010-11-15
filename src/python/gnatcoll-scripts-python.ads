-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                     Copyright (C) 2003-2010, AdaCore              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with GNATCOLL.Python;  use GNATCOLL.Python;

package GNATCOLL.Scripts.Python is

   Python_Name : constant String := "python";

   procedure Register_Python_Scripting
     (Repo          : access Scripts.Scripts_Repository_Record'Class;
      Module        : String;
      Program_Name  : String := "python";
      Python_Home   : String := "");
   --  All commands and classes will be added in the specified module.
   --
   --  Program_Name should be the name of the program registering Python
   --  scripting. The interpreter will resove run-time libraries relative to
   --  this executable.
   --
   --  If Python_Home is non-empty, it will be used as home, and libraries will
   --  be searched for in <Python_Home>/lib/python<version>

   procedure Unregister_Python_Scripting
     (Repo : access Scripts.Scripts_Repository_Record'Class);
   --  Mark the python scripting language as no longer valid. This should be
   --  called before your application exits, to prevent unwanted storage_error
   --  in the finalization of the application (since some class_instances might
   --  be automatically finalized after python itself was destroyed, otherwise)

   type Python_Scripting_Record is new Scripting_Language_Record with private;
   type Python_Scripting is access all Python_Scripting_Record'Class;
   pragma No_Strict_Aliasing (Python_Scripting);

   type Python_Callback_Data is new Callback_Data with private;

   function Get_Param
     (Data : Python_Callback_Data'Class; N : Positive)
      return PyObject;
   procedure Get_Param
     (Data    : Python_Callback_Data'Class;
      N       : Positive;
      Result  : out PyObject;
      Success : out Boolean);
   --  Return the N-th command line parameter, taking into account the keywords
   --  if any.
   --  The returned value is a borrowed reference and must not be DECREF'd

private

   ----------------------
   -- Python_scripting --
   ----------------------

   type Python_Scripting_Record is new Scripting_Language_Record with record
      Repo                     : Scripts_Repository;
      Finalized                : Boolean := False;
      Blocked                  : Boolean := False;
      Module                   : PyObject;
      Builtin                  : PyObject;
      Exception_Misc           : PyObject;
      Exception_Missing_Args   : PyObject;
      Exception_Invalid_Arg    : PyObject;
      Exception_Unexpected     : PyObject;

      Globals                  : PyObject;
      --  The global symbols for the python interpreter

      Use_Secondary_Prompt     : Boolean := False;
      --  Which type of prompt should be displayed

      Buffer                   : GNAT.Strings.String_Access;
      --  Buffer for the command, to be added in front of any command before
      --  executing. This is used for multi-line input

      In_New_Instance          : Boolean := False;
      --  Whether we are creating a new instance of a class.
      --  This is used to disable the call to __init__ (for backward
      --  compatibility and because we wouldn't know how to pass extra
      --  arguments to New_Instance).

      In_Process               : Boolean := False;
      --  True while we are processing a command. This is used to control the
      --  behavior of control-c: either interrupt, or copy

      Current_File             : Ada.Strings.Unbounded.Unbounded_String;
      --  The script we are currently executing
   end record;

   overriding function Command_Line_Treatment
     (Script : access Python_Scripting_Record) return Command_Line_Mode;
   overriding procedure Destroy (Script : access Python_Scripting_Record);
   overriding procedure Block_Commands
     (Script : access Python_Scripting_Record; Block  : Boolean);
   overriding procedure Register_Command
     (Script        : access Python_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   overriding procedure Register_Class
     (Script : access Python_Scripting_Record;
      Name   : String;
      Base   : Class_Type := No_Class);
   overriding function Create
     (Script          : access Python_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   overriding function New_Instance
     (Script : access Python_Scripting_Record;
      Class : Class_Type) return Class_Instance;
   overriding procedure Execute_Command
     (Script       : access Python_Scripting_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   overriding function Execute_Command
     (Script       : access Python_Scripting_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   overriding function Execute_Command
     (Script      : access Python_Scripting_Record;
      CL          : Arg_List;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean;
   overriding function Execute_Command
     (Script  : access Python_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return String;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return Any_Type;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return PyObject;
   --  Need to unref the returned value

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return Boolean;
   overriding procedure Execute_File
     (Script      : access Python_Scripting_Record;
      Filename    : String;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Show_Command : Boolean := True;
      Errors      : out Boolean);
   overriding function Get_Name
     (Script : access Python_Scripting_Record) return String;
   overriding function Get_Repository
     (Script : access Python_Scripting_Record) return Scripts_Repository;
   overriding function Current_Script
     (Script : access Python_Scripting_Record) return String;
   overriding procedure Set_Default_Console
     (Script       : access Python_Scripting_Record;
      Console      : Virtual_Console);
   overriding procedure Display_Prompt
     (Script  : access Python_Scripting_Record;
      Console : Virtual_Console := null);
   overriding function Interrupt
     (Script : access Python_Scripting_Record) return Boolean;
   overriding procedure Complete
     (Script      : access Python_Scripting_Record;
      Input       : String;
      Completions : out String_Lists.List);
   --  See doc from inherited subprograms

   type PyObject_Array is array (Natural range <>) of PyObject;
   type PyObject_Array_Access is access PyObject_Array;

   type Python_Callback_Data is new Callback_Data with record
      Script           : Python_Scripting;

      Args, Kw         : PyObject;
      --  Args is a tuple, a list, or any iterable

      Return_Value     : PyObject;
      Return_Dict      : PyObject;
      Has_Return_Value : Boolean := False;
      Return_As_List   : Boolean := False;
      Kw_Params        : PyObject_Array_Access;
      Is_Method        : Boolean;
   end record;
   --  Kw_Params is used to handle keyword parameters. They map from positional
   --  index to the actual object.

   overriding function Clone
     (Data : Python_Callback_Data) return Callback_Data'Class;
   overriding function Get_Script
     (Data : Python_Callback_Data) return Scripting_Language;
   overriding function Number_Of_Arguments
     (Data : Python_Callback_Data) return Natural;
   overriding procedure Name_Parameters
     (Data  : in out Python_Callback_Data; Names : Cst_Argument_List);
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return String;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Integer;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Boolean;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Subprogram_Type;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Default : String)
      return String;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Default : Integer)
      return Integer;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Default : Boolean)
      return Boolean;
   overriding function Nth_Arg
     (Data    : Python_Callback_Data;
      N       : Positive;
      Class   : Class_Type := Any_Class;
      Default : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance;
   overriding function Nth_Arg
     (Data    : Python_Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive)
      return List_Instance'Class;
   overriding procedure Set_Error_Msg
     (Data : in out Python_Callback_Data; Msg : String);
   overriding procedure Set_Return_Value_As_List
     (Data  : in out Python_Callback_Data;
      Size  : Natural := 0;
      Class : Class_Type := No_Class);
   overriding procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : Integer);
   overriding procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : String);
   overriding procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : Boolean);
   overriding procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : Class_Instance);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : String;
      Append : Boolean := False);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Integer;
      Append : Boolean := False);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False);
   overriding procedure Free (Data : in out Python_Callback_Data);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data; N : Positive; Value : String);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data; N : Positive; Value : Integer);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data; N : Positive; Value : Boolean);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data;
      N : Positive; Value : Class_Instance);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data;
      N : Positive; Value : Subprogram_Type);
   --  See doc from inherited subprogram

end GNATCOLL.Scripts.Python;
