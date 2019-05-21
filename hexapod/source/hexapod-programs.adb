package body Hexapod.Programs is

   -----------------
   -- Create_Step --
   -----------------

   function Create_Step
     (From   : Position;
      To     : Position;
      Height : Distance;
      Up     : Duration;
      Down   : Duration) return Program_Item_Array
   is
      From_Up : constant Position := (From.X, From.Y, From.Z + Height);
      To_Up   : constant Position := (To.X, To.Y, To.Z + Height);
      Way     : constant Program_Item_Array (1 .. 4) :=
        ((Tick   => Down,
          Target => To,
          Linear => True),
         (Tick   => Up * 0.1,
          Target => To_Up,
          Linear => False),
         (Tick   => Up * 0.8,
          Target => From_Up,
          Linear => False),
         (Tick   => Up * 0.1,
          Target => From,
          Linear => False));
   begin
      return Way;
   end Create_Step;

end Hexapod.Programs;
