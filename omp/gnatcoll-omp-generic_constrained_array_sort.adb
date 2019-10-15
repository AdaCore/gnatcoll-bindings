------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with Ada.Unchecked_Deallocation;
with System;

procedure GNATCOLL.OMP.Generic_Constrained_Array_Sort
  (Container : in out Array_Type)
is
   type CB_Struct is record
      Merge, Insertion_Sort : System.Address;
   end record with Convention => C;

   procedure Merge_Sort
     (Container : in out Array_Type;
      Temp      : in out Array_Type;
      Callbacks : System.Address;
      First     : Long_Integer;
      Last      : Long_Integer);
   pragma Import (C, Merge_Sort, "gnatcoll_omp_merge_sort");
   --  The merge sort driver implemented in C, using OpenMP primitives

   procedure Merge
     (Container      : in out Array_Type;
      Tmp            : in out Array_Type;
      I1, J1, I2, J2 : Long_Integer) with Convention => C;
   --  The core merge algorithm used by merge sort.
   --  Used as a callback from Merge_Sort

   procedure Insertion_Sort
     (Container   : in out Array_Type;
      First, Last : Long_Integer) with Convention => C;
   --  Insertion sort algorithm, used as a callback from Merge_Sort for small
   --  arrays.

   --------------------
   -- Insertion_Sort --
   --------------------

   procedure Insertion_Sort
     (Container   : in out Array_Type;
      First, Last : Long_Integer)
   is
      pragma Suppress (All_Checks);
   begin
      for J in Index_Type'Base'Val (First) .. Index_Type'Base'Val (Last) loop
         declare
            V : Element_Type renames Container (J);
            K : Index_Type'Base := Index_Type'Base'Pred (J);
         begin
            while K >= Index_Type'Base'Val (First)
              and then not (Container (K) < V)
            loop
               Container (Index_Type'Succ (K)) := Container (K);
               K := Index_Type'Base'Pred (K);
            end loop;

            Container (Index_Type'Succ (K)) := V;
         end;
      end loop;
   end Insertion_Sort;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Container      : in out Array_Type;
      Tmp            : in out Array_Type;
      I1, J1, I2, J2 : Long_Integer)
   is
      pragma Suppress (All_Checks);

      I, J, K : Index_Type'Base;
   begin
      I := Index_Type'Base'Val (I1);  --  beginning of the first list
      J := Index_Type'Base'Val (I2);  --  beginning of the second list
      K := I;

      while I <= Index_Type'Base'Val (J1)
        and then J <= Index_Type'Base'Val (J2)
      loop

      --  While elements in both lists

         if Container (I) < Container (J) then
            Tmp (K) := Container (I);
            K := Index_Type'Base'Succ (K);
            I := Index_Type'Base'Succ (I);
         else
            Tmp (K) := Container (J);
            K := Index_Type'Base'Succ (K);
            J := Index_Type'Base'Succ (J);
         end if;
      end loop;

      --  Copy remaining elements of the first list

      while I <= Index_Type'Base'Val (J1) loop
         Tmp (K) := Container (I);
         K := Index_Type'Base'Succ (K);
         I := Index_Type'Base'Succ (I);
      end loop;

      --  Copy remaining elements of the second list

      while J <= Index_Type'Base'Val (J2) loop
         Tmp (K) := Container (J);
         K := Index_Type'Base'Succ (K);
         J := Index_Type'Base'Succ (J);
      end loop;

      --  Transfer elements from Tmp back to Container

      for Ind in Index_Type'Base'Val (I1) .. Index_Type'Base'Val (J2) loop
         Container (Ind) := Tmp (Ind);
      end loop;
   end Merge;

   type Array_Type_Access is access all Array_Type;
   procedure Free is new
     Ada.Unchecked_Deallocation (Array_Type, Array_Type_Access);

   Tmp       : Array_Type_Access;
   Callbacks : aliased CB_Struct := (Merge'Address, Insertion_Sort'Address);

begin
   Tmp := new Array_Type;
   Merge_Sort
     (Container,
      Tmp.all,
      Callbacks'Address,
      Index_Type'Pos (Container'First),
      Index_Type'Pos (Container'Last));
   Free (Tmp);
end GNATCOLL.OMP.Generic_Constrained_Array_Sort;
