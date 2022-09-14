------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

with GNATCOLL.GMP.Integers.Misc; use GNATCOLL.GMP.Integers.Misc;

with Interfaces.C.Strings;

package body GNATCOLL.GMP.Rational_Numbers is

   use GNATCOLL.GMP.Lib;

   ------------------
   -- Canonicalize --
   ------------------

   procedure Canonicalize (This : in out Rational) is
   begin
      if not This.Canonicalized then
         mpq_canonicalize (This.Value'Access);
         This.Canonicalized := True;
      end if;
   end Canonicalize;

   ------------------
   -- Is_Canonical --
   ------------------
   function Is_Canonical (This : Rational) return Boolean is
   begin
      return This.Canonicalized;
   end Is_Canonical;

   ---------
   -- Set --
   ---------

   procedure Set
     (This         : out Rational;
      To           : Rational;
      Canonicalize : Boolean := True) is
   begin
      This.Canonicalized := To.Canonicalized;
      mpq_set (This.Value'Access, To.Value'Access);

      if Canonicalize then
         This.Canonicalize;
      end if;
   end Set;

   procedure Set (This : out Rational; To : Big_Integer) is
   begin
      --  This is canonical for sure as the denominator is set to 1 by
      --  construction.

      This.Canonicalized := True;

      mpq_set_z (This.Value'Access, As_mpz_t (To));
   end Set;

   procedure Set
     (This         : out Rational;
      Num          : Long;
      Den          : Unsigned_Long := 1;
      Canonicalize : Boolean       := True) is
   begin
      if Den = 0 then
         raise Failure with "cannot set number with 0 as denominator";
      end if;

      This.Canonicalized := False;

      mpq_set_si (This.Value'Access, Num, Den);

      if Canonicalize then
         This.Canonicalize;
      end if;
   end Set;

   procedure Set
     (This         : out Rational;
      To           : String;
      Base         : Int     := 10;
      Canonicalize : Boolean := True)
   is
      use Interfaces.C.Strings;

      Result : Int;
      Input  : chars_ptr := New_String (To);
   begin
      This.Canonicalized := False;
      Result := mpq_set_str (This.Value'Access, Input, Base);
      Free (Input);
      if Result /= 0 then
         raise Failure
           with "cannot parse " & To
             & " (base: " & Base'Image & ")";
      end if;

      if This.Denominator = 0 then
         raise Failure with "cannot set number with 0 as denominator";
      end if;

      if Canonicalize then
         This.Canonicalize;
      end if;
   end Set;

   procedure Set (This : out Rational; To : Double) is
   begin
      if To /= To then
         raise Failure with "cannot set number from a NaN";
      elsif To > Double'Last or else To < Double'First then
         raise Failure with "cannot set number from infinity";
      end if;

      --  Set from Double is canonical by construction

      This.Canonicalized := True;

      mpq_set_d (This.Value'Access, To);
   end Set;

   ----------
   -- Swap --
   ----------

   procedure Swap (R1, R2 : in out Rational) is
      Canonicalized : constant Boolean := R1.Canonicalized;
   begin
      mpq_swap (R1.Value'Access, R2.Value'Access);

      R1.Canonicalized := R2.Canonicalized;
      R2.Canonicalized := Canonicalized;
   end Swap;

   -----------
   -- Image --
   -----------

   function Image (This : Rational; Base : Integer := 10) return String is
      use Interfaces.C, Interfaces.C.Strings;

      Num    : constant Big_Integer := Numerator (This);
      Den    : constant Big_Integer := Denominator (This);
      Result : chars_ptr;

      Number_Digits : constant size_t :=
        mpz_sizeinbase (As_mpz_t (Num), Int (abs Base))
        + mpz_sizeinbase (As_mpz_t (Den), Int (abs Base));

      Buffer : String (1 .. Integer (Number_Digits) + 3);
      --  The correct number to allocate is 3 more than Number_Digits in
      --  order to handle a possible minus sign, possible slash, and the
      --  null-terminator.
   begin
      Result := mpq_get_str (Buffer'Address, Int (Base), This.Value'Access);
      return Value (Result);
   end Image;

   ---------------
   -- To_Double --
   ---------------

   function To_Double (This : Rational) return Double is
   begin
      return mpq_get_d (This.Value'Access);
   end To_Double;

   --------------------------
   -- Operand_Precondition --
   --------------------------

   procedure Operand_Precondition (This : Rational; Name : String := "") is
   begin
      if not This.Canonicalized then
         raise Failure
           with (if Name /= "" then Name & " " else Name)
             & "operand must be canonicalized";
      end if;
   end Operand_Precondition;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Rational) return Rational is
   begin
      return Result : Rational do
         Operand_Precondition (Left, "Left");
         Operand_Precondition (Right, "Right");
         mpq_add (Result.Value'Access, Left.Value'Access, Right.Value'Access);
      end return;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Rational) return Rational is
   begin
      return Result : Rational do
         Operand_Precondition (Left, "Left");
         Operand_Precondition (Right, "Right");
         mpq_sub (Result.Value'Access, Left.Value'Access, Right.Value'Access);
      end return;
   end "-";

   function "-" (Left : Rational) return Rational is
   begin
      return Result : Rational do
         Operand_Precondition (Left);
         mpq_neg (Result.Value'Access, Left.Value'Access);
      end return;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Rational) return Rational is
   begin
      return Result : Rational do
         Operand_Precondition (Left, "Left");
         Operand_Precondition (Right, "Right");
         mpq_mul (Result.Value'Access, Left.Value'Access, Right.Value'Access);
      end return;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Rational) return Rational is
   begin
      return Result : Rational do
         Operand_Precondition (Left, "Left");
         Operand_Precondition (Right, "Right");

         --  Since GNAT signal handlers can be disabled, do not rely on the
         --  runtime to raise a Contraint_Error (SIGFPE), but raise an explicit
         --  Failure exception on division by zero.

         --  Use Result as a temporary to compare Right to 0 since Rational
         --  numbers are set to 0/1 by default.

         if mpq_equal (Right.Value'Access, Result.Value'Access) = 0 then
            mpq_div (Result.Value'Access,
                     Left.Value'Access,
                     Right.Value'Access);
         else
            raise Failure with "Division by zero";
         end if;
      end return;
   end "/";

   -----------
   -- "abs" --
   -----------

   function "abs" (Left : Rational) return Rational is
   begin
      return Result : Rational do
         Operand_Precondition (Left);
         mpq_abs (Result.Value'Access, Left.Value'Access);
      end return;
   end "abs";

   ----------
   -- "**" --
   ----------

   function "**" (Left : Rational; Right : Big_Integer) return Rational is
      R : constant Unsigned_Long := Unsigned_Long (abs As_Signed_Long (Right));
   begin
      Operand_Precondition (Left, "Left");

      if not Fits_Signed_Long (Right) then
         raise Failure
           with "Exponent too big, exponentiation won't fit in memory";
      end if;

      return Result : Rational do
         if Right = 0 then
            Result.Set (1);
         else
            if Right < 0 then
               --  1/Left ** -Right

               Result.Set_Num (Left.Denominator ** (R));
               Result.Set_Den (Left.Numerator ** (R));
            else
               --  Left ** Right

               Result.Set_Num (Left.Numerator ** R);
               Result.Set_Den (Left.Denominator ** R);
            end if;

            Result.Canonicalize;
         end if;
      end return;
   end "**";

   -------
   -- = --
   -------

   function "=" (Left, Right : Rational) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      Operand_Precondition (Right, "Right");
      return mpq_equal (Left.Value'Access, Right.Value'Access) /= 0;
   end "=";

   function "=" (Left : Rational; Right : Big_Integer) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      return mpq_cmp_z (Left.Value'Access, As_mpz_t (Right)) = 0;
   end "=";

   function "=" (Left : Big_Integer; Right : Rational) return Boolean is
   begin
      Operand_Precondition (Right, "Right");
      return mpq_cmp_z (Right.Value'Access, As_mpz_t (Left)) = 0;
   end "=";

   -------
   -- > --
   -------

   function ">" (Left, Right : Rational) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      Operand_Precondition (Right, "Right");
      return mpq_cmp (Left.Value'Access, Right.Value'Access) > 0;
   end ">";

   function ">" (Left : Rational; Right : Big_Integer) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      return mpq_cmp_z (Left.Value'Access, As_mpz_t (Right)) > 0;
   end ">";

   function ">" (Left : Big_Integer; Right : Rational) return Boolean is
   begin
      Operand_Precondition (Right, "Right");
      return mpq_cmp_z (Right.Value'Access, As_mpz_t (Left)) < 0;
   end ">";

   -------
   -- < --
   -------

   function "<" (Left, Right : Rational) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      Operand_Precondition (Right, "Right");
      return mpq_cmp (Left.Value'Access, Right.Value'Access) < 0;
   end "<";

   function "<" (Left : Rational; Right : Big_Integer) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      return mpq_cmp_z (Left.Value'Access, As_mpz_t (Right)) < 0;
   end "<";

   function "<" (Left : Big_Integer; Right : Rational) return Boolean is
   begin
      Operand_Precondition (Right, "Right");
      return mpq_cmp_z (Right.Value'Access, As_mpz_t (Left)) > 0;
   end "<";

   --------
   -- >= --
   --------

   function ">=" (Left, Right : Rational) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      Operand_Precondition (Right, "Right");
      return mpq_cmp (Left.Value'Access, Right.Value'Access) >= 0;
   end ">=";

   function ">=" (Left : Rational; Right : Big_Integer) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      return mpq_cmp_z (Left.Value'Access, As_mpz_t (Right)) >= 0;
   end ">=";

   function ">=" (Left : Big_Integer; Right : Rational) return Boolean is
   begin
      Operand_Precondition (Right, "Right");
      return mpq_cmp_z (Right.Value'Access, As_mpz_t (Left)) <= 0;
   end ">=";

   --------
   -- <= --
   --------

   function "<=" (Left, Right : Rational) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      Operand_Precondition (Right, "Right");
      return mpq_cmp (Left.Value'Access, Right.Value'Access) <= 0;
   end "<=";

   function "<=" (Left : Rational; Right : Big_Integer) return Boolean is
   begin
      Operand_Precondition (Left, "Left");
      return mpq_cmp_z (Left.Value'Access, As_mpz_t (Right)) <= 0;
   end "<=";

   function "<=" (Left : Big_Integer; Right : Rational) return Boolean is
   begin
      Operand_Precondition (Right, "Right");
      return mpq_cmp_z (Right.Value'Access, As_mpz_t (Left)) >= 0;
   end "<=";

   ---------------
   -- Numerator --
   ---------------

   function Numerator (This : Rational) return Big_Integer is
      Num : aliased mpz_t;
   begin
      return Result : Big_Integer do
         mpz_init (Num'Access);
         mpq_get_num (Num'Access, This.Value'Access);
         Set (Result, Num'Access);
         mpz_clear (Num'Access);
      end return;
   end Numerator;

   -----------------
   -- Denominator --
   -----------------

   function Denominator (This : Rational) return Big_Integer is
      Den : aliased mpz_t;
   begin
      return Result : Big_Integer do
         mpz_init (Den'Access);
         mpq_get_den (Den'Access, This.Value'Access);
         Set (Result, Den'Access);
         mpz_clear (Den'Access);
      end return;
   end Denominator;

   -------------
   -- Set_Num --
   -------------

   procedure Set_Num
     (This         : in out Rational;
      Num          : Big_Integer;
      Canonicalize : Boolean := True) is
   begin
      mpq_set_num (This.Value'Access, As_mpz_t (Num));
      This.Canonicalized := False;

      if Canonicalize then
         This.Canonicalize;
      end if;
   end Set_Num;

   -------------
   -- Set_Den --
   -------------

   procedure Set_Den
     (This         : in out Rational;
      Den          : Big_Integer;
      Canonicalize : Boolean := True) is
   begin
      if Den = 0 then
         raise Failure with "cannot set denominator to 0";
      end if;

      mpq_set_den (This.Value'Access, As_mpz_t (Den));
      This.Canonicalized := False;

      if Canonicalize then
         This.Canonicalize;
      end if;
   end Set_Den;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Rational) is
   begin
      mpq_init (This.Value'Access);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Rational) is
   begin
      mpq_clear (This.Value'Access);
   end Finalize;

end GNATCOLL.GMP.Rational_Numbers;
