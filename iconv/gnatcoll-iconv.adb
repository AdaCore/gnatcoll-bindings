------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNATCOLL.Traces;          use GNATCOLL.Traces;

package body GNATCOLL.Iconv is
   Me : constant Trace_Handle := Create ("ICONV");

   C_E2BIG  : constant Integer;
   --  C_EINVAL : constant Integer;
   C_EILSEQ : constant Integer;
   pragma Import (C, C_E2BIG,  "gnatcoll_errno_e2big");
   --  pragma Import (C, C_EINVAL, "gnatcoll_errno_einval");
   pragma Import (C, C_EILSEQ, "gnatcoll_errno_eilseq");
   --  C errno values, defined in iconv_support.c

   function C_Iconv
     (cd           : System.Address;
      inbuf        : access chars_ptr;
      inbytesleft  : access size_t;
      outbuf       : access chars_ptr;
      outbytesleft : access size_t) return size_t;
   pragma Import (C, C_Iconv, "gnatcoll_iconv");

   function C_Iconv_Open (tocode, fromcode : chars_ptr) return System.Address;
   pragma Import (C, C_Iconv_Open, "gnatcoll_iconv_open");

   procedure C_Iconv_Close (State : System.Address);
   pragma Import (C, C_Iconv_Close, "gnatcoll_iconv_close");

   function Conv is new Ada.Unchecked_Conversion (System.Address, chars_ptr);

   ----------------
   -- Iconv_Open --
   ----------------

   function Iconv_Open
      (To_Code         : String := UTF8;
       From_Code       : String := Locale;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False) return Iconv_T
   is
      use System;

      State            : Iconv_T;
      Tocode, Fromcode : chars_ptr;

   begin
      if Transliteration then
         if Ignore then
            Tocode := New_String (To_Code & "//TRANSLIT//IGNORE");
         else
            Tocode := New_String (To_Code & "//TRANSLIT");
         end if;

      else
         if Ignore then
            Tocode := New_String (To_Code & "//IGNORE");
         else
            Tocode := New_String (To_Code);
         end if;
      end if;

      State.Emulate_Ignore := Ignore;

      Fromcode := New_String (From_Code);
      State.T := C_Iconv_Open (Tocode, Fromcode);
      Free (Fromcode);
      Free (Tocode);

      if State.T = Null_Address then
         raise Unsupported_Conversion with
            "Unsupported conversion from '" & From_Code & "' to '"
            & To_Code & "'";
      end if;

      return State;
   end Iconv_Open;

   -----------
   -- Iconv --
   -----------

   procedure Iconv
      (State        : Iconv_T;
       Inbuf        : Byte_Sequence;
       Input_Index  : in out Positive;
       Outbuf       : in out Byte_Sequence;
       Output_Index : in out Positive;
       Result       : out Iconv_Result)
   is
      Inptr   : aliased chars_ptr := Conv (Inbuf (Input_Index)'Address);
      Inleft  : aliased size_t := size_t (Inbuf'Last - Input_Index + 1);
      Outptr  : aliased chars_ptr := Conv (Outbuf (Output_Index)'Address);
      Outleft : aliased size_t := size_t (Outbuf'Last - Output_Index + 1);
      Res     : size_t;

   begin
      Res := C_Iconv
        (State.T, Inptr'Access, Inleft'Access, Outptr'Access, Outleft'Access);

      Input_Index := Inbuf'Last - Integer (Inleft) + 1;
      Output_Index := Outbuf'Last - Integer (Outleft) + 1;

      if Res = -1 then
         if Errno = C_EILSEQ then
            if State.Emulate_Ignore then
               Result := Full_Buffer;
               Input_Index := Input_Index + 1;
            else
               Result := Invalid_Multibyte_Sequence;
            end if;
         elsif Errno = C_E2BIG then
            Result := Full_Buffer;
         else  --  C_EINVAL
            Result := Incomplete_Multibyte_Sequence;
         end if;

      else
         Result := Success;
      end if;
   end Iconv;

   -----------
   -- Reset --
   -----------

   procedure Reset (State : Iconv_T) is
      Res : size_t;
      pragma Unreferenced (Res);
   begin
      Res := C_Iconv (State.T, null, null, null, null);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset
      (State        : Iconv_T;
       Outbuf       : in out Byte_Sequence;
       Output_Index : in out Positive;
       Result       : out Iconv_Result)
   is
      Outptr  : aliased chars_ptr := Conv (Outbuf (Output_Index)'Address);
      Outleft : aliased size_t := size_t (Outbuf'Last - Output_Index + 1);
      Res     : size_t;
   begin
      Res := C_Iconv (State.T, null, null, Outptr'Access, Outleft'Access);
      Output_Index := Outbuf'Last - Integer (Outleft) + 1;

      if Res = -1 then
         Result := Full_Buffer;
      else
         Result := Success;
      end if;
   end Reset;

   -----------------
   -- Iconv_Close --
   -----------------

   procedure Iconv_Close (State : Iconv_T) is
      use type System.Address;
   begin
      if State.T /= System.Null_Address then
         C_Iconv_Close (State.T);
      end if;
   end Iconv_Close;

   -----------
   -- Iconv --
   -----------

   function Iconv
     (State         : Iconv_T;
      Input         : Byte_Sequence;
      Ignore_Errors : Boolean := False) return Byte_Sequence
   is
      Output       : String_Access := new Byte_Sequence (1 .. Input'Length);
      Tmp          : String_Access;
      Input_Index  : Positive := Input'First;
      Output_Index : Positive := Output'First;
      Res          : Iconv_Result;
      Increment    : Integer;
   begin
      while Input_Index <= Input'Last loop
         Iconv (State, Input, Input_Index, Output.all, Output_Index, Res);
         case Res is
            when Success =>
               return R : constant String :=
                  Output (Output'First .. Output_Index - 1)
               do
                  Free (Output);
               end return;

            when Incomplete_Multibyte_Sequence =>
               if Ignore_Errors then
                  Trace (Me, "Incomplete sequence");
                  return R : constant String :=
                     Output (Output'First .. Output_Index - 1)
                  do
                     Free (Output);
                  end return;
               else
                  raise Incomplete_Sequence_Error with
                    "Incomplete sequence in '" & Input & "'";
               end if;

            when Invalid_Multibyte_Sequence =>
               Free (Output);
               if Ignore_Errors then
                  Trace (Me, "Invalid sequence");
                  return R : constant String :=
                     Output (Output'First .. Output_Index - 1)
                  do
                     Free (Output);
                  end return;
               else
                  raise Invalid_Sequence_Error with
                    "Invalid sequence in '" & Input & "'";
               end if;

            when Full_Buffer =>
               --  We might receive this because an invalid sequence was seen
               --  and the libiconv does not support //IGNORE. So we do not
               --  want to grow the string too much every time. In UTF-8, a
               --  character can take up to 6 bytes.

               if Output_Index >= Output'Last - 6 then
                  Increment := Integer'Max (1, Input'Last - Input_Index + 1);
                  Tmp := new String
                     (Output'First .. Output'Last + Increment * 2);
                  Tmp (Output'Range) := Output.all;
                  Free (Output);
                  Output := Tmp;
               end if;
         end case;
      end loop;

      return R : constant String :=
        Output (Output'First .. Output_Index - 1)
      do
         Free (Output);
      end return;
   end Iconv;

   -----------
   -- Iconv --
   -----------

   function Iconv
      (Input           : Byte_Sequence;
       To_Code         : String := UTF8;
       From_Code       : String := Locale;
       Ignore_Errors   : Boolean := False;
       Transliteration : Boolean := False;
       Ignore          : Boolean := False) return Byte_Sequence
   is
      State : Iconv_T;
   begin
      State := Iconv_Open
        (To_Code         => To_Code,
         From_Code       => From_Code,
         Transliteration => Transliteration,
         Ignore          => Ignore);

      return R : constant String :=
         Iconv (State, Input, Ignore_Errors => Ignore_Errors)
      do
         Iconv_Close (State);
      end return;

   exception
      when others =>
         Iconv_Close (State);
         raise;
   end Iconv;

   ---------------
   -- Has_Iconv --
   ---------------

   function Has_Iconv return Boolean is
   begin
      return True;
   end Has_Iconv;

end GNATCOLL.Iconv;
