with Ada.Numerics;

package Hexapod.Legs is

   type Leg is tagged limited private;
   --  Hexapod leg representation

   type Distance is new Integer;
   --  Distance in millimeters

   type Segments is record
      S1, S2, S3 : Distance;  --  Length of a segment of the leg
   end record;
   --  Each leg has 3 segments (S1, S2, S3). Here is back view
   --
   --      Leg origin        +C
   --             \       S2/ \ S3
   --   BODY       \A      /   \
   --   ============|-----+B    \
   --               ^  S1        \
   --               |             \
   --              -z              \
   --               |               .    . is touch point, leg end
   --               v<----- y ----->


   not overriding procedure Configure
     (Self     : in out Leg;
      Segments : Hexapod.Legs.Segments);

   π : constant := Ada.Numerics.Pi;

   type Angle is digits 5 range -π .. π;

   type Segment_Angles is record
      S1, S2, S3 : Angle range 0.0 .. π;  --  Angle of each segment of the leg
      --  S1 => A, S2 => B, S3 => C on the picture of back view
   end record;

   type Leg_Position is record
      X, Y, Z : Distance range -230 .. 230;
      --  Position of leg end relater to the leg origin
      --   ^z
      --   |
      --   .---> y
      --  /
      --  x
   end record;

   not overriding procedure Compute_Angles
     (Self     : Leg;
      Position : Leg_Position;
      Angles   : out Segment_Angles);
   --  Compute angles in segments for provided leg position.

private
   type Leg is tagged limited record
      Segments : Hexapod.Legs.Segments;
   end record;
end Hexapod.Legs;
