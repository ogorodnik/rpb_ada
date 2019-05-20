package body Hexapod.Schedulers is

   ------------------
   -- Add_Listener --
   ------------------

   not overriding procedure Add_Listener
     (Self : in out Scheduler;
      Item : Listener_Access;
      Time : Ada.Calendar.Time) is
   begin
      Self.Last := Self.Last + 1;
      Self.Map.Insert ((Time, Self.Last), Item);
   end Add_Listener;

   --------------------
   -- Call_Callbacks --
   --------------------

   not overriding procedure Call_Callbacks
     (Self : in out Scheduler;
      Time : in out Ada.Calendar.Time)
   is
      use type Ada.Calendar.Time;

      Item : Listener_Access;
   begin
      while not Self.Map.Is_Empty
        and then Self.Map.First_Key.Time <= Time
      loop
         Item := Self.Map.First_Element;
         Self.Map.Delete_First;
         Item.Callback (Time);
      end loop;
   end Call_Callbacks;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Key_In_Map) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return Left.Time < Right.Time
        or else Left.Unique < Right.Unique;
   end Less;

   ---------------------
   -- Remove_Listener --
   ---------------------

   not overriding procedure Remove_Listener
     (Self : in out Scheduler;
      Item : Listener_Access)
   is
      Pos : Listeners_Maps.Cursor := Self.Map.First;
   begin
      while Listeners_Maps.Has_Element (Pos) loop
         if Listeners_Maps.Element (Pos) = Item then
            Self.Map.Delete (Pos);
            exit;
         end if;

         Listeners_Maps.Next (Pos);
      end loop;
   end Remove_Listener;

end Hexapod.Schedulers;
