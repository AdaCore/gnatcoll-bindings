with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with Test_Common;

function Test return Integer
is
   Repository : Scripts_Repository := null;
   Python     : Python_Scripting   := null;
   Errors     : Boolean;

   procedure My_Class_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   ----------------------
   -- My_Class_Handler --
   ----------------------

   procedure My_Class_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
      pragma Unreferenced (Data, Command);
   begin
      null;
   end My_Class_Handler;
begin
   Test_Common.Set_Python_Home;

   Repository := new Scripts_Repository_Record;
   Register_Python_Scripting
     (Repo        => Repository,
      Module      => "Test");
   Python := GNATCOLL.Scripts.Python.Python_Scripting
     (GNATCOLL.Scripts.Lookup_Scripting_Language
        (Repository, Python_Name));

   declare
      My_Class : constant Class_Type := Repository.New_Class ("My_Class");
   begin
      Repository.Register_Command
        (Command       => "ada_method",
         Handler       => My_Class_Handler'Unrestricted_Access,
         Class         => My_Class,
         Static_Method => False);

      Repository.Register_Command
        (Command       => "ada_static",
         Handler       => My_Class_Handler'Unrestricted_Access,
         Class         => My_Class,
         Static_Method => True);
   end;

   Python.Execute_File
     (Filename     => "my_test.py",
      Show_Command => False,
      Errors       => Errors);
   Python.Destroy;

   return 0;
end Test;
