------------------------------------------------------------------------------
--                             M O D E L I N G                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

--  ??? Other implementations: no pooling (connection/close each time)
--  ??? From sqlalchemy, a parameter use_threadlocal to always return the same
--      connection when one has been checked out in the thread already.
--  ??? sqlalchemy allows overflows in pools
--  ??? sqlalchemy provides a SingletonThreadPool where a connection is only
--      returned in the thread that was used to create it (for sqlite)

with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL;        use GNATCOLL.SQL;
with GNATCOLL.SQL.Orm;
with GNATCOLL.Traces;     use GNATCOLL.Traces;
with GNATCOLL.Scripts;

package body GNATCOLL.SQL.Sessions is
   Me : constant Trace_Handle := Create ("Session", Off);
   Me_Info : constant Trace_Handle := Create ("Session.Info");

   use Element_Cache, Pointers;

   Default_Fact              : Element_Factory := Null_Factory'Access;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Detached_Element'Class, Detached_Element_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (User_Data'Class, User_Data_Access);

   function Is_Dirty (Data : Detached_Data_Access) return Boolean;
   pragma Inline (Is_Dirty);
   --  Return True if Data has been modified in memory, and not synchronized
   --  yet with the database

   procedure Set_Clean (Self : Session_Type'Class; C : Element_Cache.Cursor);
   --  Mark the element has clean, and change to a weak ref if needed.
   --  This procedure might change the contents of the cache if we were owning
   --  the last reference to the element.

   type Null_Element is new Detached_Element with null record;
   overriding function Hash (Self : Null_Element) return String;
   overriding procedure Insert_Or_Update
     (Self        : in out Null_Element;
      PK_Modified : in out Boolean;
      Dirty       : Dirty_Mask) is null;
   overriding procedure Internal_Delete (Self : Null_Element) is null;
   --  A special, always uninitialized element

   procedure Clear_Cache (Data : in out Session_Data);
   --  Clear the cache, releasing memory as appropriate

   procedure Trace_Debug
     (Me  : GNATCOLL.Traces.Trace_Handle;
      C   : Element_Cache.Cursor;
      Msg : String := "");
   --  Print information on C

   package String_Lists renames GNATCOLL.Scripts.String_Lists;
   use String_Lists;

   procedure Insert_Delete_Or_Update
     (Self      : Session_Type;
      C         : Element_Cache.Cursor;
      Tmp_List  : in out Element_List.List;
      To_Add    : in out Element_List.List;
      To_Delete : in out String_Lists.List);
   --  Flush the specific element at the given index.  To_Delete contains on
   --  exit the list of elements to remove from the cache (since we can't do
   --  that while traversing the cache).
   --  To_Add is the list of elements that should be added to the cache.
   --  Tmp_List is used to hold temporary references to elements that we are
   --  converting back to a weak reference so that it isn't removed from the
   --  cache and the latter is not modified.

   procedure Add_To_Cache (Self : Session_Type; E : Detached_Element'Class);
   --  Add the element into the cache. We want the element to outlive the
   --  session (so that we can find out all changes when committing the
   --  session), so we store a real reference.

   -----------------
   -- Ref_Or_Weak --
   -----------------

   function Get_Data (Self : Ref_Or_Weak) return Detached_Data_Access;
   pragma Inline (Get_Data);
   --  Returned the element data stored in Self

   procedure To_Real_Reference (Self : in out Ref_Or_Weak);
   --  Make Self a real reference, instead of a weak reference

   procedure To_Weak_Reference (Self : in out Ref_Or_Weak);
   --  Make Self a weak reference.
   --  This does not check the Weak_Cache setting.

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Self : Ref_Or_Weak) return Detached_Data_Access is
      Data : constant Detached_Data_Access :=
        Detached_Data_Access (Self.Ref.Get);
   begin
      if Data /= null then
         return Data;
      end if;

      return Detached_Data_Access (Get (Self.WRef).Get);
   end Get_Data;

   -----------------------
   -- To_Real_Reference --
   -----------------------

   procedure To_Real_Reference (Self : in out Ref_Or_Weak) is
   begin
      if Self.WRef /= Pointers.Null_Weak_Ref then
         Set (Self.Ref.all, Get_Data (Self));  --  increments refcount
         Self.WRef := Pointers.Null_Weak_Ref;

         if Active (Me) then
            Trace (Me, "Adding reference to " & Hash (Self.Ref.all));
         end if;
      end if;
   end To_Real_Reference;

   -----------------------
   -- To_Weak_Reference --
   -----------------------

   procedure To_Weak_Reference (Self : in out Ref_Or_Weak) is
   begin
      if Self.WRef = Pointers.Null_Weak_Ref then
         if Active (Me) then
            Trace (Me, "Adding weakref to " & Hash (Self.Ref.all));
         end if;

         Self.WRef := Pointers.Get_Weak_Ref (Self.Ref.all);
         Set (Self.Ref.all, null);  --  decrement refcount of the old value
      end if;
   end To_Weak_Reference;

   ----------
   -- Hash --
   ----------

   overriding function Hash (Self : Null_Element) return String is
      pragma Unreferenced (Self);
   begin
      return "null";
   end Hash;

   ------------------
   -- Impl_Factory --
   ------------------

   function Impl_Factory (Data : Pool_Data) return Session_Data is
      DB : constant Database_Connection := Data.Descr.Build_Connection;
   begin
      Assert
        (Me, DB /= null,
         "Could not connect to database. Wrong dbtype set in settings ?");

      return (DB                    => DB,
              Pool                  => Data.Pool,
              Cache                 => new Element_Cache.Map,
              User                  => null,
              Store_Unmodified      => Data.Config_Store_Unmodified,
              Weak_Cache            => Data.Config_Weak_Cache,
              Persist_Cascade       => Data.Config_Persist_Cascade,
              Flush_Before_Query    => Data.Config_Flush_Before_Query,
              Has_Modified_Elements => False,
              Factory               => Default_Fact);
   end Impl_Factory;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Pool_Data) is
   begin
      Free (Data.Descr);

      if Data.Config_Default_User_Data /= null then
         Free (Data.Config_Default_User_Data.all);
         Unchecked_Free (Data.Config_Default_User_Data);
      end if;
   end Free;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Data : in out Session_Data) is
      C    : Element_Cache.Cursor;
      WRef : Ref_Or_Weak;
      D    : Detached_Data_Access;
   begin
      if Data.Cache /= null then
         C := Data.Cache.First;
         while Has_Element (C) loop
            WRef := Element (C);

            if Active (Me) then
               Trace_Debug (Me, C, "Removed from cache: ");
            end if;

            --  The element no longer belongs to the session (it might actually
            --  be freed by Unchecked_Free, but we do not know that for sure)
            D := Get_Data (WRef);
            if D /= null then
               D.Session := Null_Weak_Session;
            end if;

            Unchecked_Free (WRef.Ref);
            Next (C);
         end loop;

         Data.Cache.Clear;
      end if;
   end Clear_Cache;

   ---------------------
   -- Impl_On_Release --
   ---------------------

   procedure Impl_On_Release (Data : in out Session_Data) is
   begin
      Increase_Indent (Me, "Releasing session in pool "
                       & Session_Pool'Image (Data.Pool));
      if Data.Cache /= null then
         Clear_Cache (Data);
      end if;

      if Data.DB.Automatic_Transactions then
         Rollback (Data.DB, "");  --  Release any pending transaction
      end if;

      if Data.User /= null then
         Trace (Me, "Freeing session data");
         Free (Data.User.all);
         Unchecked_Free (Data.User);
      end if;

      Data.Has_Modified_Elements := False;

      Decrease_Indent (Me, "Done releasing session");
   end Impl_On_Release;

   ---------------
   -- Impl_Free --
   ---------------

   procedure Impl_Free (Data : in out Session_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Element_Cache.Map'Class, Map_Access);
   begin
      if Active (Me) then
         Trace (Me, "Freeing a session and its cache, closing SQL connection"
                & " in pool " & Session_Pool'Image (Data.Pool));
      end if;
      Clear_Cache (Data);  --  Should have been done in Impl_On_Release
      Unchecked_Free (Data.Cache);

      Data.Factory := Default_Fact;

      Free (Data.DB);  --  Close connection
   end Impl_Free;

   ---------------------
   -- Get_New_Session --
   ---------------------

   function Get_New_Session
     (Pool : Session_Pool := Default_Pool) return Session_Type
   is
      Self : Session_Type;
   begin
      if Active (Me) then
         Trace (Me, "Getting new session from pool "
                & Session_Pool'Image (Pool));
      end if;
      Impl.Get (Self, Set => Pool);
      Reset_Connection (Self.DB);
      return Self;
   end Get_New_Session;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Descr              : GNATCOLL.SQL.Exec.Database_Description;
      Max_Sessions       : Positive;
      Default_User_Data  : User_Data'Class := No_User_Data;
      Store_Unmodified   : Boolean := False;
      Weak_Cache         : Boolean := True;
      Flush_Before_Query : Boolean := True;
      Persist_Cascade    : Boolean := True;
      Pool               : Session_Pool := Default_Pool)
   is
   begin
      Impl.Set_Factory
        (Pool_Data'
           (Descr                     => Descr,
            Pool                      => Pool,
            Config_Store_Unmodified   => Store_Unmodified,
            Config_Weak_Cache         => Weak_Cache,
            Config_Flush_Before_Query => Flush_Before_Query,
            Config_Default_User_Data  =>
               new User_Data'Class'(Default_User_Data),
            Config_Persist_Cascade    => Persist_Cascade),
         Max_Sessions, Set => Pool);
   end Setup;

   -------------------------
   -- Set_Default_Factory --
   -------------------------

   procedure Set_Default_Factory (Factory : Element_Factory) is
   begin
      Default_Fact := Factory;
   end Set_Default_Factory;

   -----------------
   -- Set_Factory --
   -----------------

   procedure Set_Factory
     (Self    : in out Session_Type'Class;
      Factory : Element_Factory) is
   begin
      Self.Element.Factory := Factory;
   end Set_Factory;

   ------------------
   -- Null_Factory --
   ------------------

   function Null_Factory
     (From    : Base_Element'Class;
      Default : Detached_Element'Class) return Detached_Element'Class
   is
      pragma Unreferenced (From);
   begin
      return Default;
   end Null_Factory;

   -------------
   -- Factory --
   -------------

   function Factory
      (Self    : Session_Type'Class;
       From    : Base_Element'Class;
       Default : Detached_Element'Class) return Detached_Element'Class is
   begin
      return Self.Element.Factory (From, Default);
   end Factory;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Detached_Element) return Boolean is
   begin
      return Self.Get = null;
   end Is_Null;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Detached_Data) is
      C    : Element_Cache.Cursor;
      WRef : Ref_Or_Weak;
      Session : constant Session_Type := Get (Self.Session);
   begin
      --  This procedure is only called for elements that have a weak-ref (or
      --  no ref at all) in a session. If the session is holding a real
      --  reference, Self cannot be freed anyway.
      --  The goal here is to save memory by removing the element's weak ref
      --  from the session cache. This is not mandatory, since the weak ref
      --  remains usable, but ensures memory usage does not grow up too much.

      --  This will actually free Self, and properly reset all weakref to it.
      --  At this point, the weak references in the cache that are no longer
      --  pointing to anything can simply be removed. There is only one such
      --  weak ref (entry was Self), so we can stop iterating early.
      --  This call might result in recursive calls to Free.

      Free (Weak_Refcounted (Self));

      if Session /= No_Session then
         C := Session.Element.Cache.First;
         while Has_Element (C) loop
            WRef := Element (C);

            if WRef.WRef /= Pointers.Null_Weak_Ref
              and then Was_Freed (WRef.WRef)
            then
               if Active (Me) then
                  Trace (Me, "Removing from cache: freed weakref "
                         & Element_Cache.Key (C));
               end if;

               Unchecked_Free (WRef.Ref);
               Session.Element.Cache.Delete  (C);
               exit;
            end if;
            Next (C);
         end loop;

         --  We used to check that the element was found in the cache. But in
         --  fact, when an element is removed after its primary key has changed
         --  (on an INSERT for instance), we will not find it anymore in the
         --  cache.
      end if;
   end Free;

   -----------------
   -- Trace_Debug --
   -----------------

   procedure Trace_Debug
     (Me  : GNATCOLL.Traces.Trace_Handle;
      C   : Element_Cache.Cursor;
      Msg : String := "")
   is
      D     : constant Ref_Or_Weak := Element (C);
      Data  : constant Detached_Data_Access := Get_Data (D);
      Count : Integer;
   begin
      if D.WRef = Pointers.Null_Weak_Ref then
         Count := Get_Refcount (D.Ref.all);
         if not Active (Me) and then Count /= 1 then
            --  Always show when an element has remaining references, since it
            --  helps debugging memory issues.
            Trace (Me_Info, Msg & " (remaining refs to '" & Key (C)
                   & "' refcount=" & Count'Img & ")");
         end if;

         if Data = null then
            Trace (Me, Msg & "ref to deallocated '" & Key (C)
                   & "' refcount=" & Count'Img);
         elsif Is_Dirty (Data) then
            Trace (Me, Msg & "ref to modified '" & Key (C)
                   & "' refcount=" & Count'Img);
         else
            Trace (Me, Msg & "ref to unmodified '" & Key (C)
                   & "' refcount=" & Count'Img);
         end if;
      else
         if Was_Freed (D.WRef) then
            Trace (Me, Msg & "weakref to freed '" & Key (C) & "'");
         else
            Trace (Me, Msg & "weakref to '" & Key (C) & "'");
         end if;
      end if;
   end Trace_Debug;

   -----------------
   -- Trace_Debug --
   -----------------

   procedure Trace_Debug
     (Me      : GNATCOLL.Traces.Trace_Handle;
      Session : Session_Type;
      Msg     : String := "")
   is
      C    : Element_Cache.Cursor := Session.Element.Cache.First;
   begin
      Increase_Indent (Me, Msg);
      Trace (Me, "Refcount=" & Get_Refcount (Session)'Img);
      while Has_Element (C) loop
         Trace_Debug (Me, C, "");
         Next (C);
      end loop;

      Decrease_Indent (Me, "Done " & Msg);

   exception
      when E : others =>
         Decrease_Indent (Me, "Done " & Msg & " Unexpected exception");
         Trace (Me, E);
   end Trace_Debug;

   ----------------
   -- From_Cache --
   ----------------

   procedure From_Cache
     (Self    : Session_Type;
      Key     : String;
      Element : out Detached_Element_Access;
      Data    : out Detached_Data_Access)
   is
      C : Element_Cache.Cursor;
      R : Ref_Or_Weak;
   begin
      C := Self.Element.Cache.Find (Key);
      if Has_Element (C) then
         R := Element_Cache.Element (C);
         Element := R.Ref;
         Data := Get_Data (R);
         if Data = null then
            --  We had a weakref, but the corresponding element was freed.
            Element := null;
         end if;

      else
         Element := null;
         Data    := null;
      end if;
   end From_Cache;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
     (Self : Detached_Element; Field : Natural)
   is
      D : constant Detached_Data_Access := Detached_Data_Access (Self.Get);
      C : Element_Cache.Cursor;
      Session : Session_Type;
      R       : Ref_Or_Weak;
      Was_Dirty : constant Boolean := Is_Dirty (D);
   begin
      if Active (Me) then
         Trace (Me, "Set_Modified, Field=" & Field'Img
                & " Was_Dirty=" & Was_Dirty'Img);
      end if;

      D.Dirty (Field) := True;

      if not Was_Dirty then

         --  Converts the weak-ref to an actual ref in the cache, if needed
         --  We first need to find the element in the cache, though

         Session := Get (D.Session);
         if Session /= No_Session then
            declare
               H : constant String :=
                 Hash (Detached_Element'Class (Self));
            begin
               C := Session.Element.Cache.Find (H);
               if C /= Element_Cache.No_Element then
                  R := Element_Cache.Element (C);
                  To_Real_Reference (R);
                  Trace (Me, "Set_Modified, replacing element key=" & H);
                  Session.Element.Cache.Replace_Element (C, R);

                  Session.Element.Has_Modified_Elements := True;

               else
                  --  If it is modified, it must belong to a session
                  Session.Element.Has_Modified_Elements := True;
                  Session.Persist (Self);
               end if;
            end;
         end if;
      end if;
   end Set_Modified;

   --------
   -- DB --
   --------

   function DB (Self : Session_Type) return Database_Connection is
   begin
      return Self.Element.DB;
   end DB;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      Impl.Free;
   end Free;

   --------------
   -- Get_Weak --
   --------------

   function Get_Weak (Self : Session_Type) return Weak_Session is
      W : Impl.Weak_Resource;
   begin
      W := Impl.Get_Weak (Impl.Resource (Self));
      return Weak_Session'(Ref => W);
   end Get_Weak;

   ---------
   -- Get --
   ---------

   function Get (Self : Weak_Session) return Session_Type is
      Result : Session_Type;
   begin
      Impl.Get (Self.Ref, Result);
      return Result;
   end Get;

   -------------
   -- Persist --
   -------------

   procedure Persist
     (Self : Session_Type; Element : Detached_Element'Class)
   is
      D : constant Detached_Data_Access := Detached_Data_Access (Element.Get);
   begin
      if D = null then
         return;
      end if;

      --  Store a weak-reference to the session in the element, so that it can
      --  be used to query further attributes, or register changes
      --  The Element can outlive the session, so we store a weak-reference.

      if Get (D.Session) /= No_Session then
         if Get (D.Session) /= Self then
            raise Already_Persistent
              with "Element already belongs to another session";
         end if;

         --  Element is already in the session, but might not be in the cache
         --  if it wasn't modified before and Config_Store_Unmodified is False.
      else
         D.Session := Get_Weak (Self);
      end if;

      if not Self.Element.Store_Unmodified then
         if not Is_Dirty (D) then
            return;
         end if;

         Self.Element.Has_Modified_Elements := True;
      else
         Self.Element.Has_Modified_Elements :=
           Self.Element.Has_Modified_Elements or else Is_Dirty (D);
      end if;

      Add_To_Cache (Self, Element);
   end Persist;

   ------------------
   -- Add_To_Cache --
   ------------------

   procedure Add_To_Cache (Self : Session_Type; E : Detached_Element'Class) is
      H : constant String := Hash (E);
      R : Ref_Or_Weak :=
        (Ref     => new Detached_Element'Class'(E),
         WRef    => Pointers.Null_Weak_Ref);
      Inserted : Boolean;
      Pos : Element_Cache.Cursor;
   begin
      if Self.Element.Weak_Cache
        and then not Is_Dirty (Detached_Data_Access (E.Get))
      then
         To_Weak_Reference (R);
      else
         To_Real_Reference (R);
      end if;

      Self.Element.Cache.Insert
        (Key => H, New_Item => R, Position => Pos, Inserted => Inserted);
      if Inserted then
         if Active (Me) then
            Trace (Me, "Adding to session: " & H
                   & " has_modified_elems: "
                   & Self.Element.Has_Modified_Elements'Img);
         end if;
         On_Persist (E);
      else
         Unchecked_Free (R.Ref);
      end if;
   end Add_To_Cache;

   --------------
   -- Is_Dirty --
   --------------

   function Is_Dirty (Data : Detached_Data_Access) return Boolean is
   begin
      for D in Data.Dirty'Range loop
         if Data.Dirty (D) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Dirty;

   ---------------
   -- Set_Clean --
   ---------------

   procedure Set_Clean
     (Self : Session_Type'Class; C : Element_Cache.Cursor)
   is
      D    : Ref_Or_Weak := Element (C);
      Data : constant Detached_Data_Access := Get_Data (D);
   begin
      if Data /= null then
         for D in Data.Dirty'Range loop
            Data.Dirty (D) := False;
         end loop;

         if Self.Element.Weak_Cache then
            To_Weak_Reference (D);
            Self.Element.Cache.Replace_Element (C, D);
         end if;
      end if;
   end Set_Clean;

   -----------------
   -- Cache_Count --
   -----------------

   procedure Cache_Count
     (Self    : Session_Type;
      Refs    : out Natural;
      Weakref : out Natural)
   is
      C    : Element_Cache.Cursor := Self.Element.Cache.First;
   begin
      Refs    := 0;
      Weakref := 0;

      while Has_Element (C) loop
         if Element (C).WRef = Pointers.Null_Weak_Ref then
            Refs := Refs + 1;
         else
            Weakref := Weakref + 1;
         end if;

         Next (C);
      end loop;
   end Cache_Count;

   ---------------------
   -- Persist_Cascade --
   ---------------------

   function Persist_Cascade (Self : Session_Type) return Boolean is
   begin
      return Self /= No_Session
        and then Self.Element.Persist_Cascade;
   end Persist_Cascade;

   ----------------------
   -- Insert_Or_Update --
   ----------------------

   procedure Insert_Or_Update
     (Self    : Session_Type;
      Element : in out Detached_Element'Class)
   is
      H : constant String := Hash (Element);
      C : constant Element_Cache.Cursor := Self.Element.Cache.Find (H);
   begin
      if Has_Element (C) then
         Flush (Self);
      else
         Trace (Me, "Insert_Or_Update: element " & H & " not in cache");
      end if;
   end Insert_Or_Update;

   -----------------------------
   -- Insert_Delete_Or_Update --
   -----------------------------

   procedure Insert_Delete_Or_Update
     (Self      : Session_Type;
      C         : Element_Cache.Cursor;
      Tmp_List  : in out Element_List.List;
      To_Add    : in out Element_List.List;
      To_Delete : in out String_Lists.List)
   is
      D    : constant Ref_Or_Weak := Element (C);
      PK_Modified : Boolean := False;
   begin
      --  We only need to look at actual references: if we still have a
      --  weak ref, that means the element has not been modified

      if D.WRef /= Pointers.Null_Weak_Ref
        or else not Is_Dirty (Get_Data (D))
      then
         return;
      end if;

      if Active (Me) then
         Trace_Debug (Me, C, "Flush: ");
      end if;

      --  Mark the element as clean, and convert the reference back to a
      --  weak reference since the element is no longer modified.
      --
      --  If we owned the last reference to the element, it will be freed
      --  and thus removed from the cache. We can no longer iterate on
      --  the cache afterward. If we did not own the last reference, the
      --  element will not be freed, and neither will any other element
      --  that it might have an access to.
      --
      --  As a solution, we create a temporary reference in Tmp, so that
      --  Set_Clean will never actually free the element. When we Clear
      --  Tmp, however, the last reference might be terminated, and the
      --  elements removed from the list.

      declare
         R : Detached_Element'Class :=
           Detached_Element'Class'(D.Ref.all);
         Dirty : constant Dirty_Mask := Get_Data (D).Dirty;
         Old_Hash : constant String := Key (C);
      begin
         Set (R, Get_Data (D));
         Tmp_List.Append (R);

         --  Reset the dirty mask, to prevent infinite recursion when an
         --  element depends (possibly indirectly) on itself

         Set_Clean (Self, C);

         if Dirty (Dirty_Mask_Deleted) then
            Internal_Delete (R);
            To_Delete.Append (Old_Hash);  --  Will remove from cache
         else
            Insert_Or_Update (R, PK_Modified, Dirty);

            if PK_Modified then
               if Hash (R) /= Old_Hash then
                  To_Add.Append (R);  --  Will insert in cache
                  To_Delete.Append (Old_Hash);  --  Will remove from cache
               end if;
            end if;
         end if;
      end;
   end Insert_Delete_Or_Update;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : Session_Type; Element : Detached_Element'Class)
   is
      pragma Unreferenced (Self);
   begin
      Element.Set_Modified (Dirty_Mask_Deleted);
   end Delete;

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : Session_Type) is
      use Element_List;

      C    : Element_Cache.Cursor := Self.Element.Cache.First;
      To_Delete : String_Lists.List;
      C2   : String_Lists.Cursor;
      To_Add, Tmp_List : Element_List.List;
      C3 : Element_List.Cursor;
   begin
      if Self.Element.Has_Modified_Elements then
         Increase_Indent (Me, "Flushing session");
         while Has_Element (C) loop
            Insert_Delete_Or_Update
              (Self, C, Tmp_List => Tmp_List, To_Add => To_Add,
               To_Delete => To_Delete);
            Next (C);
         end loop;

         if not Tmp_List.Is_Empty then
            Trace (Me, "Add element to cache, since the hash has changed");
            C3 := Tmp_List.First;
            while Has_Element (C3) loop
               Add_To_Cache (Self, E => Element_List.Element (C3));
               Next (C3);
            end loop;
         end if;

         if not To_Delete.Is_Empty then
            Trace (Me, "Removing elements from the cache");
            C2 := To_Delete.First;
            while Has_Element (C2) loop
               Self.Element.Cache.Delete (String_Lists.Element (C2));
               Next (C2);
            end loop;
         end if;

         Decrease_Indent (Me, "Done flushing session");
         Self.Element.Has_Modified_Elements := False;
      end if;

      --  Do not clear the cache.
      --  The elements are already synchronized in memory anyway, so we
      --  might as well preserve them.
      --  ??? The only delicate area is when you have default values set
      --  by the database, in which case we might not have the value in
      --  memory and will not fetch it next time.

      --  Clear_Cache (Get (Self));

   exception
      when GNATCOLL.SQL.Orm.Self_Referencing =>
         Decrease_Indent (Me, "Got self-referencing");
         raise;

      when E : others =>
         Trace (Me, E);
         Decrease_Indent (Me, "while in flushing session");
   end Flush;

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (Self : Session_Type) is
   begin
      if not In_Transaction (Self.DB) then
         Execute (Self.DB, SQL_Begin);
      end if;
   end Begin_Transaction;

   ------------
   -- Commit --
   ------------

   procedure Commit (Self : Session_Type) is
   begin
      Flush (Self);
      if In_Transaction (Self.DB) then
         Execute (Self.DB, SQL_Commit);
      end if;
   end Commit;

   --------------
   -- Rollback --
   --------------

   procedure Rollback (Self : Session_Type) is
   begin
      Trace (Me, "Rollback session");
      if In_Transaction (Self.DB) then
         Execute (Self.DB, SQL_Rollback);
      end if;

      Clear_Cache (Self.Element.all);
   end Rollback;

   -------------
   -- Session --
   -------------

   function Session (Self : Detached_Element'Class) return Session_Type is
   begin
      return Get (Detached_Data_Access (Self.Get).Session);
   end Session;

   ----------------------
   -- Get_Weak_Session --
   ----------------------

   function Get_Weak_Session
     (Self : Detached_Element'Class) return Weak_Session is
   begin
      return Detached_Data_Access (Self.Get).Session;
   end Get_Weak_Session;

   ------------------------
   -- Flush_Before_Query --
   ------------------------

   function Flush_Before_Query (Self : Session_Type) return Boolean is
   begin
      return Self.Element.Flush_Before_Query;
   end Flush_Before_Query;

   ---------------------------
   -- Set_Default_User_Data --
   ---------------------------

   procedure Set_Default_User_Data
     (Default_User_Data  : User_Data'Class := No_User_Data;
      Pool               : Session_Pool := Default_Pool)
   is
      P : constant access Pool_Data := Impl.Get_Factory_Param (Pool);
   begin
      Free (P.Config_Default_User_Data.all);
      Unchecked_Free (P.Config_Default_User_Data);
      P.Config_Default_User_Data := new User_Data'Class'(Default_User_Data);
   end Set_Default_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Self : Session_Type; Pool : Session_Pool := Default_Pool)
      return access User_Data'Class
   is
      D : constant access Session_Data := Self.Element;
      P : access Pool_Data;
   begin
      if D.User = null then
         P := Impl.Get_Factory_Param (Pool);
         if P.Config_Default_User_Data /= null then
            D.User := new User_Data'Class'(P.Config_Default_User_Data.all);
         end if;
      end if;

      return D.User;
   end Get_User_Data;

   ------------
   -- Delete --
   ------------

   procedure Delete (Element : Detached_Element) is
   begin
      Element.Session.Delete (Element);
   end Delete;

end GNATCOLL.SQL.Sessions;
