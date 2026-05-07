with Ada.Strings.Fixed;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with Test_Assert;
with Test_Common;

function Test return Integer is
   Repository : Scripts_Repository := null;
   Python     : Python_Scripting := null;
   Errors     : Boolean;

   procedure Check_Backtrace_Handler
     (Data : in out Callback_Data'Class; Command : String);

   -----------------------------
   -- Check_Backtrace_Handler --
   -----------------------------

   procedure Check_Backtrace_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Data, Command);
      BT : constant String := Python_Backtrace;
   begin
      --  This handler is called from Python code in my_test.py, so
      --  Last_Call_Frame points to a frame whose code object lives in
      --  my_test.py. The backtrace walk exercises the reference-counting
      --  fix for PyFrame_Get_Code / PyFrame_Get_Back.
      Test_Assert.Assert
        (BT'Length > 0, "Python_Backtrace should return a non-empty string");

      Test_Assert.Assert
        (Ada.Strings.Fixed.Index (BT, "my_test.py") > 0,
         "Backtrace should mention my_test.py, got: " & BT);
   end Check_Backtrace_Handler;

begin
   Test_Common.Set_Python_Home;

   Repository := new Scripts_Repository_Record;
   Register_Python_Scripting (Repo => Repository, Module => "Test");
   Python :=
     GNATCOLL.Scripts.Python.Python_Scripting
       (GNATCOLL.Scripts.Lookup_Scripting_Language (Repository, Python_Name));

   Repository.Register_Command
     (Command       => "check_backtrace",
      Handler       => Check_Backtrace_Handler'Unrestricted_Access,
      Static_Method => False);

   Python.Execute_File
     (Filename => "my_test.py", Show_Command => False, Errors => Errors);

   Test_Assert.Assert
     (not Errors, "The python script should not have raised an error");

   Python.Destroy;
   return Test_Assert.Report;
end Test;
