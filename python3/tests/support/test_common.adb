with Ada.Environment_Variables;

package body Test_Common is

   package Env renames Ada.Environment_Variables;

   function Python_Home return String is
   begin
      return Env.Value ("ADA_PYTHON_HOME");
   end Python_Home;

end Test_Common;
