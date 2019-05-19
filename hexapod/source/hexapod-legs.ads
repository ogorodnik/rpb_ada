with Ada.Numerics;

package Hexapod.Legs is

   type Leg is tagged limited private;
   --  Hexapod leg representation

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

   π : constant := Ada.Numerics.Pi;

   type Angle is digits 5 range -π .. π;

   not overriding procedure Configure
     (Self     : in out Leg;
      Segments : Hexapod.Legs.Segments;
      Origin   : Position;
      Rotated  : Angle);
   --  Configure the leg - set segments' lengths and leg origin in the space.
   --  Origin is Position of the first leg segment.
   --  Rotated is an angle of the first leg segment in default position,
   --  zero means the segment is parallel to the Y axe.

   type Segment_Angles is record
      S1, S2, S3 : Angle range 0.0 .. π;  --  Angle of each segment of the leg
      --  S1 => A, S2 => B, S3 => C on the picture of back view
   end record;

   not overriding procedure Compute_Angles
     (Self     : Leg;
      Position : Hexapod.Legs.Position;
      Angles   : out Segment_Angles);
   --  Compute angles in segments for provided leg position.
   --  Position of leg end related to the leg origin (see Configure above).

private
   type Leg is tagged limited record
      Segments : Hexapod.Legs.Segments;
      Origin   : Hexapod.Legs.Position;
      Rotated  : Angle;
   end record;
end Hexapod.Legs;
