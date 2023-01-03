------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2023, AdaCore                     --
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

--  This package provides a direct interface to the GNU Multiple Precision
--  numeric library implementation in the C language. The names of the routines
--  and types follow those of the C implementation. In addition, the intended
--  semantics of the types are actually enforced by the Ada compiler.
--  Specifically, C programmers are not intended to perform copying via
--  assignment or comparisons via predefined equality. We use a limited private
--  type to enforce those expectations. Note that the low-level Ada numeric
--  types used in the interfaces are defined in the root Ada package named GMP.

--  Ada programmers can program at this level (i.e., using this package) but
--  are intended to use the higher level interfaces defined in other child
--  packages. For example, arbitrary precision integers are supported by
--  package GMP.Integers and children of that package.

--  The C library uses a single shared namespace and types reference each other
--  as needed, such that a single package is the most direct, clean way to
--  model it. The entire GMP library at the C language level is intended to be
--  defined here, but not all routines are currently defined. Other routine
--  interfaces can be added as needed, in the future.

--  Individual major sections of the GMP library C binding are separated by
--  comment lines containing the names of the corresponding sections. For
--  example, the "Integer Functions" section is defined first, followed by
--  "Random Number Functions". These section names correspond to the GMP
--  library documentation. In addition, each major section is further divided
--  in to subsections with comments indicating the name of the subsection.
--  These subsection names also correspond to the names used in the GMP library
--  documentation. For example, the "Integer Functions" section is subdivided
--  into "Initialization", "Assignment", "Combined Initialization and
--  Assignment", and so forth. Ada programmers can, therefore, use the GMP
--  library documentation and more easily find the required C binding (if they
--  must program at this level).

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C_Streams;

with System;

package GNATCOLL.GMP.Lib is

   pragma Preelaborate;

   type mpz_t is limited private;
   --  Following the C interface, clients are not intended to access the
   --  fields of this type, for the sake of upward compatibility. Nor are they
   --  intended to do simple copying via assignment or comparisons via
   --  predefined equality. We use a limited private type to enforce those
   --  expectations.

   type gmp_randstate_t is limited private;
   --  Following the C interface, clients are not intended to access the
   --  fields of this type for the sake of upward compatibility. Nor are they
   --  intended to do simple copying via assignment or comparisons via
   --  predefined equality. We use a limited private type to enforce those
   --  expectations.

   type mpq_t is limited private;
   --  Following the C interface, clients are not intended to access the fields
   --  of this type, for the sake of upward compatibility. Nor are they
   --  intended to do simple copying via assignment or comparisons via
   --  predefined equality. We use a limited private type to enforce those
   --  expectations.

--  Integer Functions ---------------------------------------------------------

   --  Initialization

   procedure mpz_init (this : access mpz_t);
   pragma Import (C, mpz_init, "__gmpz_init");

   procedure mpz_clear (this : access mpz_t);
   pragma Import (C, mpz_clear, "__gmpz_clear");

   --  Assignment

   procedure mpz_set (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_set, "__gmpz_set");

   procedure mpz_set_si (ROP : access mpz_t;  OP : Long);
   pragma Import (C, mpz_set_si, "__gmpz_set_si");

   procedure mpz_set_ui (ROP : access mpz_t;  OP : Unsigned_Long);
   pragma Import (C, mpz_set_ui, "__gmpz_set_ui");

   function mpz_set_str (this : access mpz_t;  str : chars_ptr; base : Int)
      return Int;
   pragma Import (C, mpz_set_str, "__gmpz_set_str");

   procedure mpz_swap (ROP1, ROP2 : access mpz_t);
   pragma Import (C, mpz_swap, "__gmpz_swap");

   --  Combined initialization and assignment

   procedure mpz_init_set (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_init_set, "__gmpz_init_set");

   procedure mpz_init_set_si (ROP : access mpz_t;  OP : Long);
   pragma Import (C, mpz_init_set_si, "__gmpz_init_set_si");

   function mpz_init_set_str (ROP : access mpz_t;  STR : chars_ptr; BASE : Int)
      return Int;
   pragma Import (C, mpz_init_set_str, "__gmpz_init_set_str");

   --  Conversion

   function mpz_get_str (STR  : System.Address;
                         BASE : Int;
                         OP   : access constant mpz_t)
      return chars_ptr;
   pragma Import (C, mpz_get_str, "__gmpz_get_str");

   function mpz_get_si (OP : access constant mpz_t) return Long;
   pragma Import (C, mpz_get_si, "__gmpz_get_si");

   --  Arithmetic

   procedure mpz_add (ROP : access mpz_t;  OP1, OP2 : access constant mpz_t);
   pragma Import (C, mpz_add, "__gmpz_add");

   procedure mpz_add_ui (ROP : access mpz_t;
                         OP1 : access constant mpz_t;
                         OP2 : Unsigned_Long);
   pragma Import (C, mpz_add_ui, "__gmpz_add_ui");

   procedure mpz_sub (ROP : access mpz_t;  OP1, OP2 : access constant mpz_t);
   pragma Import (C, mpz_sub, "__gmpz_sub");

   procedure mpz_sub_ui (ROP : access mpz_t;
                         OP1 : access constant mpz_t;
                         OP2 : Unsigned_Long);
   pragma Import (C, mpz_sub_ui, "__gmpz_sub_ui");

   procedure mpz_mul (ROP : access mpz_t;  OP1, OP2 : access constant mpz_t);
   pragma Import (C, mpz_mul, "__gmpz_mul");

   procedure mpz_mul_si (ROP : access mpz_t;
                         OP1 : access constant mpz_t;
                         OP2 : Long);
   pragma Import (C, mpz_mul_si, "__gmpz_mul_si");

   procedure mpz_neg (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_neg, "__gmpz_neg");

   procedure mpz_abs (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_abs, "__gmpz_abs");

   --  Division

   procedure mpz_tdiv_q (Q : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_tdiv_q, "__gmpz_tdiv_q");

   function mpz_tdiv_q_ui (Q : access mpz_t;
                           N : access constant mpz_t;
                           D : Unsigned_Long)
      return Long;  -- returns the remainder but puts the quotient in Q
   pragma Import (C, mpz_tdiv_q_ui, "__gmpz_tdiv_q_ui");

   procedure mpz_tdiv_r (R : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_tdiv_r, "__gmpz_tdiv_r");
   --   R will have the same sign as N.

   function mpz_tdiv_r_ui (R : access mpz_t;
                           N : access constant mpz_t;
                           D : Unsigned_Long)
      return Long;  --  return value is the absolute value of the remainder
   pragma Import (C, mpz_tdiv_r_ui, "__gmpz_tdiv_r_ui");

   procedure mpz_mod (R : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_mod, "__gmpz_mod");
   --  result is always non-negative

   procedure mpz_fdiv_q (Q : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_fdiv_q, "__gmpz_fdiv_q");

   procedure mpz_cdiv_q (Q : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_cdiv_q, "__gmpz_cdiv_q");

   procedure mpz_fdiv_r (Q : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_fdiv_r, "__gmpz_fdiv_r");

   procedure mpz_cdiv_r (Q : access mpz_t;  N, D : access constant mpz_t);
   pragma Import (C, mpz_cdiv_r, "__gmpz_cdiv_r");

   --  Logical and Bit Manipulation

   procedure mpz_and (Rop : access mpz_t;  Op1, Op2 : access constant mpz_t);
   pragma Import (C, mpz_and, "__gmpz_and");

   procedure mpz_ior (Rop : access mpz_t;  Op1, Op2 : access constant mpz_t);
   pragma Import (C, mpz_ior, "__gmpz_ior");

   procedure mpz_xor (Rop : access mpz_t;  Op1, Op2 : access constant mpz_t);
   pragma Import (C, mpz_xor, "__gmpz_xor");

   procedure mpz_com (Rop : access mpz_t;  Op : access constant mpz_t);
   pragma Import (C, mpz_com, "__gmpz_com");

   --  Exponentiation

   procedure mpz_pow_ui (ROP : access mpz_t;
                         BASE : access constant mpz_t;
                         EXP : Unsigned_Long);
   pragma Import (C, mpz_pow_ui, "__gmpz_pow_ui");

   --  Root Extraction

   procedure mpz_sqrt (ROP : access mpz_t; OP : access constant mpz_t);
   pragma Import (C, mpz_sqrt, "__gmpz_sqrt");

   function mpz_root
      (ROP : access mpz_t; OP : access constant mpz_t; N : Unsigned_Long)
      return Int;
   pragma Import (C, mpz_root, "__gmpz_root");

   procedure mpz_sqrtrem
     (ROP1 : access mpz_t;
      ROP2 : access mpz_t;
      OP   : access constant mpz_t);
   pragma Import (C, mpz_sqrtrem, "__gmpz_sqrtrem");

   procedure mpz_rootrem
     (ROOT       : access mpz_t;
      REMAINDER  : access mpz_t;
      U          : access constant mpz_t;
      N          : Unsigned_Long);
   pragma Import (C, mpz_rootrem, "__gmpz_rootrem");

   --  Number Theoretic

   procedure mpz_gcd (ROP : access mpz_t;  Op1, Op2 : access constant mpz_t);
   pragma Import (C, mpz_gcd, "__gmpz_gcd");

   --  Comparison

   function mpz_cmp (OP1, OP2 : access constant mpz_t) return Int;
   pragma Import (C, mpz_cmp, "__gmpz_cmp");

   function mpz_cmp_si (OP1 : access constant mpz_t;  OP2 : Long) return Int;
   pragma Import (C, mpz_cmp_si, "__gmpz_cmp_si");

   function mpz_cmp_ui (OP1 : access constant mpz_t;  OP2 : Unsigned_Long)
      return Int;
   pragma Import (C, mpz_cmp_ui, "__gmpz_cmp_ui");

   function mpz_sgn (OP : access constant mpz_t) return Int;
   pragma Import (C, mpz_sgn, "gmp_mpz_sgn");
   --  our wrapper for their macro

   --  Logical and Bit Manipulation

   --  Input / Output

   function mpz_out_str (file : Interfaces.C_Streams.FILEs;
                         base : Int;
                         this : access constant mpz_t)
      return Interfaces.C.size_t;
   pragma Import (C, mpz_out_str, "__gmpz_out_str");

   --  Random

   procedure mpz_urandomb
     (ROP   : access mpz_t;
      STATE : access constant gmp_randstate_t;
      --  should really be access-to-variable to match semantics
      N     : Unsigned_Long);
   pragma Import (C, mpz_urandomb, "__gmpz_urandomb");

   procedure mpz_urandomm
     (ROP   : access mpz_t;
      STATE : access constant gmp_randstate_t;
      --  should really be access-to-variable to match semantics
      N     : access constant mpz_t);
   pragma Import (C, mpz_urandomm, "__gmpz_urandomm");

   --  Import and Export

   --  Miscellaneous

   function mpz_fits_slong_p (OP : access constant mpz_t) return Int;
   pragma Import (C, mpz_fits_slong_p, "__gmpz_fits_slong_p");

   function mpz_sizeinbase (this : access constant mpz_t; base : Int)
      return Interfaces.C.size_t;
   pragma Import (C, mpz_sizeinbase, "__gmpz_sizeinbase");

   function mpz_odd_p (OP : access constant mpz_t) return Int;
   pragma Import (C, mpz_odd_p, "gmp_mpz_odd_p");
   --  our wrapper for their macro

   function mpz_even_p (OP : access constant mpz_t) return Int;
   pragma Import (C, mpz_even_p, "gmp_mpz_even_p");
   --  our wrapper for their macro

--  Random Number Functions ---------------------------------------------------

   --  State Initialization

   procedure gmp_randinit_default (STATE : access gmp_randstate_t);
   pragma Import (C, gmp_randinit_default, "__gmp_randinit_default");

   procedure gmp_randinit_mt (STATE : access gmp_randstate_t);
   pragma Import (C, gmp_randinit_mt, "__gmp_randinit_mt");

   procedure gmp_randinit_set
     (ROP : access gmp_randstate_t;
      OP  : access constant gmp_randstate_t);
   pragma Import (C, gmp_randinit_set, "__gmp_randinit_set");

   procedure gmp_randclear (STATE : access gmp_randstate_t);
   pragma Import (C, gmp_randclear, "__gmp_randclear");

   --  State Seeding

   procedure gmp_randseed
     (STATE : access gmp_randstate_t;
      SEED  : access constant mpz_t);
   pragma Import (C, gmp_randseed, "__gmp_randseed");

   procedure gmp_randseed_ui
     (STATE : access gmp_randstate_t;
      SEED  : Unsigned_Long);
   pragma Import (C, gmp_randseed_ui, "__gmp_randseed_ui");

   --  Misc

   function gmp_urandomb_ui (STATE : access gmp_randstate_t; N : Unsigned_Long)
      return Long;
   pragma Import (C, gmp_urandomb_ui, "__gmp_urandomb_ui");

   function gmp_urandomm_ui (STATE : access gmp_randstate_t; N : Unsigned_Long)
      return Long;
   pragma Import (C, gmp_urandomm_ui, "__gmp_urandomm_ui");

--  Rational Number Functions

   procedure mpq_canonicalize (this : access constant mpq_t) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_canonicalize";
   --  Remove any factors that are common to the numerator and denominator of
   --  This, and make the denominator positive.

   --  Initialization and Assignment

   procedure mpq_init (this : access mpq_t) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_init";
   --  Initialize this and set it to 0/1. Each variable should normally only be
   --  initialized once, or at least cleared out (using the function mpq_clear)
   --  between each initialization. NOTE: since we automatically handle memory
   --  management through controlled type, we perform this check directly in
   --  the Set functions.

   procedure mpq_clear (this : access mpq_t) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_clear";
   --  Free resources allocated for this. Make sure to call this function for
   --  all mpq_t variables when you are done with them. NOTE: this is ensured
   --  by the use of a controlled type.

   procedure mpq_set (this : access mpq_t; op : access constant mpq_t) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set";
   --  Assign this from op

   procedure mpq_set_z (this : access mpq_t; op : access constant mpz_t) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_z";
   --  Assign this from op

   procedure mpq_set_ui (this : access mpq_t; op1, op2 : Unsigned_Long) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_ui";
   --  Set the value of this to op1/op2

   procedure mpq_set_si
     (this : access mpq_t;
      op1  : Long;
      op2  : Unsigned_Long)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_si";
   --  Set the value of this to op1/op2

   function mpq_set_str
     (this : access mpq_t;
      str  : chars_ptr;
      base : Int) return Int
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_str";
   --  See Rational.Set

   procedure mpq_swap (rop1, rop2 : access mpq_t) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_swap";
   --  Swap the values rop1 and rop2 efficiently

   --  Conversion

   function mpq_get_d (op : access constant mpq_t) return Double with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_get_d";
   --  Convert op to a double, truncating if necessary (i.e. rounding towards
   --  zero).
   --
   --  See Rational_Numbers.To_Double for more details.

   procedure mpq_set_d (this : access mpq_t; op : Double) with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_d";
   --  Set this to the value of op. There is no rounding, this conversion
   --  is exact.

   function mpq_get_str
     (str  : System.Address;
      base : Int;
      op   : access constant mpq_t) return chars_ptr
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_get_str";
   --  Convert op to a string of digits in base base. The base argument may
   --  vary from 2 to 62 or from -2 to -36. The string will be of the form
   --  "num/den", or if the denominator is 1 then just "num".
   --
   --  For base in the range 2..36, digits and lower-case letters are used; for
   --  -2..-36, digits and upper-case letters are used; for 37..62, digits,
   --  upper-case letters, and lower-case letters (in that significance order)
   --  are used.
   --
   --  str should point to a block of storage large enough for the result, that
   --  being
   --
   --  mpz_num_sizeinbase (op, base)
   --  + mpz_den_sizeinbase (op, base) + 3
   --
   --  The three extra bytes are for a possible minus sign, possible slash, and
   --  the null-terminator.
   --
   --  A pointer to the result string is returned, being either the allocated
   --  block, or the given str.

   --  Arithmetic

   procedure mpq_add
     (this     : access mpq_t;
      op1, op2 : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_add";

   procedure mpq_sub
     (this     : access mpq_t;
      op1, op2 : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_sub";

   procedure mpq_mul
     (this     : access mpq_t;
      op1, op2 : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_mul";

   procedure mpq_div
     (this     : access mpq_t;
      op1, op2 : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_div";

   procedure mpq_neg
     (this : access mpq_t;
      op   : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_neg";

   procedure mpq_abs
     (this : access mpq_t;
      op   : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_abs";

   --  Comparisons

   function mpq_cmp (op1, op2 : access constant mpq_t) return Int with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_cmp";

   function mpq_cmp_z
     (op1 : access constant mpq_t;
      op2 : access constant mpz_t) return Int
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_cmp_z";

   function mpq_equal (op1, op2 : access constant mpq_t) return Int with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_equal";

   --  Integer Functions

   procedure mpq_get_num
     (num : access mpz_t;
      op  : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_get_num";
   --  Get the numerator of a rational

   procedure mpq_get_den
     (den : access mpz_t;
      op  : access constant mpq_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_get_den";
   --  Get the denumerator of a rational

   procedure mpq_set_num
     (op  : access constant mpq_t;
      num : access constant mpz_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_num";
   --  Set the numerator of a rational

   procedure mpq_set_den
     (op  : access constant mpq_t;
      den : access constant mpz_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpq_set_den";
   --  Set the denumerator of a rational

private

   type mpz_t is record
      mp_alloc : Int;
      mp_size  : Int;
      mp_d     : System.Address;
   end record;
   pragma Convention (C, mpz_t);

   type gmp_randstate_t is limited record
      mp_seed : mpz_t;
      mp_alg  : Int;
      mp_lc   : System.Address;
   end record;
   pragma Convention (C, gmp_randstate_t);

   type mpq_t is record
      num : mpz_t;
      den : mpz_t;
   end record with Convention => C;

end GNATCOLL.GMP.Lib;
