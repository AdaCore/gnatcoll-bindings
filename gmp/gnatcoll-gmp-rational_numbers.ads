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

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Lib;

with Ada.Finalization;

package GNATCOLL.GMP.Rational_Numbers is

   type Rational is tagged limited private;
   --  The type is limited because clients should not use predefined
   --  assignment; nor should they use predefined equality. This matches the
   --  semantics of the underlying GMP library in C. For assignment, use the
   --  Set routines. The equality operator is explicitly redefined.

   --  The underlying C version of the GMP requires the user to manually
   --  initialize the rational number objects (i.e., those of type
   --  mpq_t). Likewise, users are expected to clear these objects to reclaim
   --  the memory allocated. Initialization and clearing are performed
   --  automatically in this Ada version.

   Failure : exception;

   procedure Canonicalize (This : in out Rational);
   --  Remove any factors that are common to the numerator and denominator of
   --  This, and make the denominator positive.
   --
   --  All rational arithmetic functions assume operands have a canonical form,
   --  and canonicalize their result. The canonical form means that the
   --  denominator and the numerator have no common factors, and that the
   --  denominator is positive. Zero has the unique representation 0/1.
   --
   --  Set procedures canonicalize the assigned variable by default (see Set
   --  methods below) so you don't have to worry about it. If you do not need
   --  explicitly to handle non-canonical values, you don't have to use this
   --  procedure, every rational number is automatically canonicalized.

   function Is_Canonical (This : Rational) return Boolean;
   --  Return whether This is in canonical form

   --  Assignment

   procedure Set
     (This         : out Rational;
      To           : Rational;
      Canonicalize : Boolean := True);
   --  Copy the To rational number to This

   procedure Set (This : out Rational; To : Big_Integer);
   --  Set This rational to a Big_Integer

   procedure Set
     (This         : out Rational;
      Num          : Long;
      Den          : Unsigned_Long := 1;
      Canonicalize : Boolean       := True);
   --  Set This rational to "Num/Den" integers

   procedure Set
     (This         : out Rational;
      To           : String;
      Base         : Int     := 10;
      Canonicalize : Boolean := True);
   --  Set This from the string To in the given Base.
   --
   --  The string can be an integer like "41" or a fraction like "41/152". The
   --  fraction must be in canonical form, or if not then Canonicalize must
   --  be called.
   --
   --  The numerator and optional denominator are parsed the same as in
   --  Integers.Set. White space is allowed in the string, and is simply
   --  ignored. The base can vary from 2 to 62, or if base is 0 then the
   --  leading characters are used: 0x or 0X for hex, 0b or 0B for binary, 0
   --  for octal, or decimal otherwise. Note that this is done separately for
   --  the numerator and denominator, so for instance 0xEF/100 is 239/100,
   --  whereas 0xEF/0x100 is 239/256.
   --
   --  Raise a Failure exception if the string is not valid or if the
   --  denominator is 0.

   procedure Set (This : out Rational; To : Double);
   --  Set This to a Double

   procedure Swap (R1, R2 : in out Rational);
   --  Swap two Rational numbers

   --  Output

   function Image (This : Rational; Base : Integer := 10) return String;
   --  See GMP.Lib.mpq_get_str for more documentation about Base

   --  Conversion

   function To_Double (This : Rational) return Double;
   --  Convert This to a double, truncating if necessary (i.e. rounding towards
   --  zero).
   --
   --  If the exponent from the conversion is too big or too small to fit a
   --  double then the result is system dependent. For too big numbers an
   --  infinity is returned when available. For too small numbers, 0.0 is
   --  normally returned. Hardware overflow, underflow and denorm traps may or
   --  may not occur.

   --  Arithmetic

   function "+" (Left, Right : Rational) return Rational;
   function "-" (Left, Right : Rational) return Rational;
   function "*" (Left, Right : Rational) return Rational;
   function "/" (Left, Right : Rational) return Rational;
   function "-" (Left : Rational) return Rational;
   function "abs" (Left : Rational) return Rational;
   function "**" (Left : Rational; Right : Big_Integer) return Rational;

   --  Comparisons

   function "=" (Left : Rational;    Right : Rational)    return Boolean;
   function "=" (Left : Rational;    Right : Big_Integer) return Boolean;
   function "=" (Left : Big_Integer; Right : Rational)    return Boolean;

   function ">" (Left : Rational;    Right : Rational)    return Boolean;
   function ">" (Left : Rational;    Right : Big_Integer) return Boolean;
   function ">" (Left : Big_Integer; Right : Rational)    return Boolean;

   function "<" (Left : Rational;    Right : Rational)    return Boolean;
   function "<" (Left : Rational;    Right : Big_Integer) return Boolean;
   function "<" (Left : Big_Integer; Right : Rational)    return Boolean;

   function ">=" (Left : Rational;    Right : Rational)    return Boolean;
   function ">=" (Left : Rational;    Right : Big_Integer) return Boolean;
   function ">=" (Left : Big_Integer; Right : Rational)    return Boolean;

   function "<=" (Left : Rational;    Right : Rational)    return Boolean;
   function "<=" (Left : Rational;    Right : Big_Integer) return Boolean;
   function "<=" (Left : Big_Integer; Right : Rational)    return Boolean;

   --  Integer functions

   function Numerator (This : Rational) return Big_Integer;
   --  Return the numerator of This

   function Denominator (This : Rational) return Big_Integer;
   --  Return the denominator of This

   procedure Set_Num
     (This         : in out Rational;
      Num          : Big_Integer;
      Canonicalize : Boolean := True);
   --  Set the numerator of This

   procedure Set_Den
     (This         : in out Rational;
      Den          : Big_Integer;
      Canonicalize : Boolean := True);
   --  Set the denominator of This

private

   type Rational is new Ada.Finalization.Limited_Controlled with
      record
         Value         : aliased GNATCOLL.GMP.Lib.mpq_t;
         Canonicalized : Boolean := True;
         --  Canonical form is required by all rational arithemtic operations.
         --  This member keeps track of whether Value was canonicalized, so
         --  that we both never do arithmetic on non-canonical forms and
         --  canonicalize values at most once. Since all rational arithemtic
         --  operations return a canonicalized result, we set it to True by
         --  default. Only some assignment functions can produce non-canonical
         --  values.
      end record;

   overriding procedure Initialize (This : in out Rational);
   overriding procedure Finalize   (This : in out Rational);

   procedure Operand_Precondition (This : Rational; Name : String := "");
   --  Check that This is in canonical form, if not, raise an exception saying
   --  that Name is not canonicalized.

end GNATCOLL.GMP.Rational_Numbers;
