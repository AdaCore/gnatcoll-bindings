with GNATCOLL.Python.Lifecycle; use GNATCOLL.Python.Lifecycle;
with GNATCOLL.Python; use GNATCOLL.Python;
with Test_Assert;
with Test_Common;

function Test return Integer is

   package A renames Test_Assert;

   Status : Interpreter_Status;
begin
   Py_SetPythonHome (Test_Common.Python_Home);
   Py_SetProgramName;
   Py_Initialize;
   Status := Py_Main;
   A.Assert(Status = Interpreter_Exit_Normally, "Interpreter failure");
   Py_Finalize;
   return A.Report;
end Test;
