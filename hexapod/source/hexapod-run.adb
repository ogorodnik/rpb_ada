with Hexapod.Legs;
with Ada.Text_IO;

procedure Hexapod.Run is
   use all type Hexapod.Legs.Angle;
   use all type Hexapod.Legs.Distance;
   package Angle_IO is new Ada.Text_IO.Float_IO (Hexapod.Legs.Angle);
   Leg : Hexapod.Legs.Leg;
   Angles : Hexapod.Legs.Segment_Angles;
begin
   Leg.Configure
     (Segments => (30, 80, 110),
      Origin => (10, 20, 30),
      Rotated => -1.0);
   Leg.Compute_Angles
     (Position => (X => 10, Y => 120, Z => -20),
      Angles   => Angles);
   Angle_IO.Put (Angles.S1);
   Angle_IO.Put (Angles.S2);
   Angle_IO.Put (Angles.S3);
end Hexapod.Run;
