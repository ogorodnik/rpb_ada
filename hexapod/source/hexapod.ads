package Hexapod is
   pragma Pure;

   type Distance is new Integer;
   --  Distance in millimeters

   type Position is record
      X, Y, Z : Distance;
   end record;
   --  Position in 3D space
   --   ^z
   --   |
   --   .---> y
   --  /
   --  x

end Hexapod;
