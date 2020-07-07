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
      Command : String)
   is
      Property_Name : constant String := "My_Property";
   begin
      if Command = Constructor_Method then
         declare
            My_Inst : constant Class_Instance := Nth_Arg (Data, 1);
            Name : constant String := Nth_Arg (Data, 2);
         begin
            Set_Data (My_Inst, Property_Name, Create_Property (Name));
         end;
      elsif Command = "Get_Property" then
         declare
            My_Inst : constant Class_Instance := Nth_Arg (Data, 1);
         begin
            Set_Return_Value
              (Data, Get_Data (My_Inst, Property_Name).As_String);
         end;
      end if;
   end My_Class_Handler;

begin
   Test_Common.Set_Python_Home;

   Repository := new Scripts_Repository_Record;
   Register_Python_Scripting
     (Repo         => Repository,
      Module       => "Test");
   Python := GNATCOLL.Scripts.Python.Python_Scripting
     (GNATCOLL.Scripts.Lookup_Scripting_Language
        (Repository, Python_Name));

   declare
      My_Class : constant Class_Type := Repository.New_Class ("My_Class");
   begin
      Repository.Register_Command
        (Command => Constructor_Method,
         Params  => (1 .. 1 => Param ("name")),
         Handler => My_Class_Handler'Unrestricted_Access,
         Class   => My_Class);

      Repository.Register_Command
        (Command       => "Get_Property",
         Handler       => My_Class_Handler'Unrestricted_Access,
         Class         => My_Class);
   end;

   Python.Execute_File
     (Filename     => "my_test.py",
      Show_Command => False,
      Errors       => Errors);
   Python.Destroy;

   return 0;
end Test;
