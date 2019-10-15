------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This is a replacement for package Generic_Sorting in Ada.Containers.Vectors
--  using OpenMP to implement the Sort procedure in parallel.

generic
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   --  The actual function for the generic formal function "<" of
   --  Generic_Sorting is expected to return the same value each time it is
   --  called with a particular pair of element values. It should define a
   --  strict ordering relationship, that is, be irreflexive, asymmetric,
   --  and transitive; it should not modify Container. If the actual for "<"
   --  behaves in some other manner, the behavior of the subprograms of
   --  Generic_Sorting are unspecified. How many times the subprograms of
   --  Generic_Sorting call "<" is unspecified.
package Ada.Containers.Vectors.Generic_Parallel_Sorting is

   function Is_Sorted (Container : Vector) return Boolean;
   --  Returns True if the elements are sorted smallest first as determined
   --  by the generic formal "<" operator; otherwise, Is_Sorted returns
   --  False. Any exception raised during evaluation of "<" is propagated.
   --  This subprogram is not using OpenMP.

   procedure Sort (Container : in out Vector);
   --  Reorders the elements of Container such that the elements are sorted
   --  smallest first as determined by the generic formal "<" operator
   --  provided. Any exception raised during evaluation of "<" is
   --  propagated.
   --  This subprogram is using OpenMP.

   procedure Merge (Target : in out Vector; Source : in out Vector);
   --  Merge removes elements from Source and inserts them into Target;
   --  afterwards, Target contains the union of the elements that were
   --  initially in Source and Target; Source is left empty. If Target and
   --  Source are initially sorted smallest first, then Target is ordered
   --  smallest first as determined by the generic formal "<" operator;
   --  otherwise, the order of elements in Target is unspecified. Any
   --  exception raised during evaluation of "<" is propagated.
   --  This subprogram is not using OpenMP.

end Ada.Containers.Vectors.Generic_Parallel_Sorting;
