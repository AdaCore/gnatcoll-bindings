with Ada.Environment_Variables;
with GNATCOLL.Python.Lifecycle; use GNATCOLL.Python.Lifecycle;

package body Test_Common is

   package Env renames Ada.Environment_Variables;

   function Python_Home return String is
   begin
      return Env.Value ("ADA_PYTHON_HOME");
   end Python_Home;

   procedure Set_Python_Home is
   begin
      Py_SetPythonHome (Python_Home);
   end Set_Python_Home;
end Test_Common;
