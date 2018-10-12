with GNATCOLL.Iconv;
with Test_Assert;

function Test return Integer is
   package Iconv renames GNATCOLL.Iconv;
   package A renames Test_Assert;

   St      : Iconv.Iconv_T;
   Success : Boolean := False;
begin
   begin
      St := Iconv.Iconv_Open ("nonexistent", "nonexistent");
   exception
      when Iconv.Unsupported_Conversion =>
         Success := True;
   end;

   A.Assert (Success, Msg => "handling of bad charset");
   return A.Report;
end Test;
