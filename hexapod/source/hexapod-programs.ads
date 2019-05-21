package Hexapod.Programs is

   --  This package provides a program, list of way points and corresponding
   --  time labels.

   type Program_Item is record
      Tick   : Duration range 0.1 .. 3_600.0;  --  Duration of program item
      Target : Position;  --  Position of the let at the end
      Linear : Boolean;
      --  Use closest to linear interpolation way if True, use any way if False
   end record;

   type Program_Item_Array is array (Positive range <>) of Program_Item;
   --  Program is a list of way points.

   function Create_Step
     (From   : Position;
      To     : Position;
      Height : Distance;
      Up     : Duration;
      Down   : Duration) return Program_Item_Array;
   --  This function creates way points for one step. From and To correspond to
   --  begin and end for the part of step, when leg is on the floor. Leg spends
   --  Down seconds to move From..To segment and Up seconds to move To..From.
   --  Leg is raised to given Height.
   --
   --   +<-----Up------+
   --   |              ^ Height
   --   v     Down     |
   --   +------------->+
   ---  From           To

end Hexapod.Programs;
