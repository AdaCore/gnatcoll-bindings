with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with Test_Common;

function Test return Integer
is
   Repository : Scripts_Repository := null;
   Python     : Python_Scripting   := null;
   Errors     : Boolean;
begin
   Test_Common.Set_Python_Home;

   Repository := new Scripts_Repository_Record;
   Register_Python_Scripting
     (Repo        => Repository,
      Module      => "Test");
   Python := GNATCOLL.Scripts.Python.Python_Scripting
     (GNATCOLL.Scripts.Lookup_Scripting_Language
        (Repository, Python_Name));

   Python.Execute_File
     (Filename     => "simple_print.py",
      Show_Command => False,
      Errors       => Errors);
   Python.Destroy;

   return 0;
end Test;
