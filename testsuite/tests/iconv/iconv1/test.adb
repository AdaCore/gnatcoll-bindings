with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Source_Info;
with GNATCOLL.Iconv;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package Iconv renames GNATCOLL.Iconv;
   package IO renames Ada.Text_IO;
   package SI renames GNAT.Source_Info;


   function Bytes_Image (Input : String) return String;
   function Bytes_Image (Input : String) return String
   is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C in Input'Range loop
         Ada.Strings.Unbounded.Append (Result, Character'Pos (Input (C))'Img);
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end Bytes_Image;

   procedure Assert_Iconv
      (Input           : String;
       Expected        : String;
       From_Code       : String;
       To_Code         : String;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False;
       Msg             : String := "";
       Location        : String := SI.Source_Location)
   is
   --  Transform Input using Iconv and expect Expected as a result
   --  The other parameters are for GNATCOLL.Iconv.Iconv
   begin
      declare
         Result : constant String := Iconv.Iconv
            (Input => Input,
             To_Code => To_Code,
             From_Code => From_Code,
             Transliteration => Transliteration,
             Ignore => Ignore);
         Success : constant Boolean := Result = Expected;
      begin
         A.Assert (Success, Msg, Location);
         if not Success then
            IO.Put_Line ("iconv(" & Bytes_Image (Input) &
                         ", to_code => " & To_Code &
                         ", from_code => " & From_code &
                         ", transliteration => " & Transliteration'Img &
                         ", ignore => " & Ignore'Img);
            IO.Put_Line ("- expect: " & Bytes_Image (Expected));

            IO.Put_Line ("- got: " & Bytes_Image (Result));
         end if;
      end;
   exception
      when E : others =>
         A.Assert (False, Msg, Location);
         IO.Put_Line ("got exception: " & ASCII.LF &
                      Ada.Exceptions.Exception_Information (E));
   end Assert_Iconv;


   procedure Assert_Invalid_Sequence
      (Input           : String;
       From_Code       : String;
       To_Code         : String;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False;
       Msg             : String := "";
       Location        : String := SI.Source_Location)
   is
   --  Assert that input sequence will be considered by Iconv as an invalid
   --  sequence.
   begin
      declare
         Result : constant String := Iconv.Iconv
            (Input => Input,
             To_Code => To_Code,
             From_Code => From_Code,
             Transliteration => Transliteration,
             Ignore => Ignore);
      begin
         A.Assert (False, Msg, Location);
         IO.Put_Line ("iconv(" & Input &
                      ", to_code => " & To_Code &
                      ", from_code => " & From_code &
                      ", transliteration => " & Transliteration'Img &
                      ", ignore => " & Ignore'Img);
         IO.Put_Line ("- expect: Invalid_Sequence_Error ");
         IO.Put ("- got: ");
         for C in Result'Range loop
            IO.Put (Character'Pos (Result (C))'Img);
         end loop;
         IO.New_Line;
      end;
   exception
      when Iconv.Invalid_Sequence_Error =>
         A.Assert (True, Msg, Location);
      when E : others =>
         A.Assert (False, Msg, Location);
         IO.Put_Line ("got exception: " & ASCII.LF &
                      Ada.Exceptions.Exception_Information (E));
   end Assert_Invalid_Sequence;


   Eacute : constant Character := Character'Val (16#E9#);
   --  in Iso_8859-1

   R_Koi8 : constant Character := Character'Val (16#D1#);
   EAcute_Koi8_Trans : constant String :=  --  a 'e
      Character'Val (39) & Character'Val (101);
   --  In KOI8-R

   Eacute_UTF8 : constant String :=
      Character'Val (195) & Character'Val (169);
   R_UTF8 : constant String :=
      Character'Val (209) & Character'Val (143);

   Ellipsis_UTF8 : constant String :=
      Character'Val (16#E2#) & Character'Val (16#80#) & Character'Val (16#A6#);

   Alveolar_Click : constant String :=
      Character'Val (16#C7#) & Character'Val (16#82#);

begin
   Assert_Iconv ("Simple test", "Simple test", "", "", Msg => "simple test");
   Assert_Iconv ("Simple " & Eacute, "Simple " & Eacute_UTF8, Iconv.Iso_8859_1, Iconv.UTF8);
   Assert_Iconv ("Simple " & R_Koi8, "Simple " & R_UTF8, Iconv.KOI8_R, Iconv.UTF8);

   Assert_Invalid_Sequence
      ("Simple " & Eacute, Iconv.Iso_8859_1, Iconv.KOI8_R);

   --  Test with invalid character both in the middle and at the end
   Assert_Iconv
      ("Simple " & Eacute, "Simple ", Iconv.Iso_8859_1, Iconv.KOI8_R,
       Ignore => True);
   Assert_Iconv
      ("T" & Eacute & "Simple", "TSimple", Iconv.Iso_8859_1, Iconv.KOI8_R,
       Ignore => True);

   Assert_Iconv
      (Input => Ellipsis_UTF8,
       Expected => "...",
       From_Code => Iconv.UTF8,
       To_Code => Iconv.ASCII,
       Transliteration => True,
       Msg => "Test transliteration on ellipsis");

   Assert_Iconv
      (Input => Ellipsis_UTF8,
       Expected => "",
       From_Code => Iconv.UTF8,
       To_Code => Iconv.ASCII,
       Transliteration => False,
       Ignore => True,
       Msg => "Test UTF-8 to ASCII of ellipsis without transliteration");

   Assert_Iconv
      (Input => Alveolar_Click & "A",
       Expected => "?" & "A",
       From_Code => Iconv.UTF8,
       To_Code => Iconv.ASCII,
       Transliteration => True,
       Ignore => True,
       Msg => "Test character for which there is no transliteration");

   return A.Report;
end Test;
