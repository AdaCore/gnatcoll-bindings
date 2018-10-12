with GNATCOLL.Iconv;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package Iconv renames GNATCOLL.Iconv;

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
   A.Assert_Iconv ("Simple test", "Simple test", "", "", Msg => "simple test");
   A.Assert_Iconv ("Simple " & Eacute, "Simple " & Eacute_UTF8, Iconv.Iso_8859_1, Iconv.UTF8);
   A.Assert_Iconv ("Simple " & R_Koi8, "Simple " & R_UTF8, Iconv.KOI8_R, Iconv.UTF8);

   A.Assert_Invalid_Sequence
      ("Simple " & Eacute, Iconv.Iso_8859_1, Iconv.KOI8_R);

   --  Test with invalid character both in the middle and at the end
   A.Assert_Iconv
      ("Simple " & Eacute, "Simple ", Iconv.Iso_8859_1, Iconv.KOI8_R,
       Ignore => True);
   A.Assert_Iconv
      ("T" & Eacute & "Simple", "TSimple", Iconv.Iso_8859_1, Iconv.KOI8_R,
       Ignore => True);

   A.Assert_Iconv
      (Input => Ellipsis_UTF8,
       Expected => "...",
       From_Code => Iconv.UTF8,
       To_Code => Iconv.ASCII,
       Transliteration => True,
       Msg => "Test transliteration on ellipsis");

   A.Assert_Iconv
      (Input => Ellipsis_UTF8,
       Expected => "",
       From_Code => Iconv.UTF8,
       To_Code => Iconv.ASCII,
       Transliteration => False,
       Ignore => True,
       Msg => "Test UTF-8 to ASCII of ellipsis without transliteration");

   A.Assert_Iconv
      (Input => Alveolar_Click & "A",
       Expected => "A",
       From_Code => Iconv.UTF8,
       To_Code => Iconv.ASCII,
       Transliteration => True,
       Ignore => True,
       Msg => "Test character for which there is no transliteration");

   return A.Report;
end Test;
