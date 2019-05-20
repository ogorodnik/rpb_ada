with Hexapod.Legs;
with Hexapod.Schedulers;

with Ada.Calendar;
with Ada.Text_IO;

procedure Hexapod.Run is
   use all type Hexapod.Legs.Angle;
   use all type Hexapod.Legs.Distance;
   use type Ada.Calendar.Time;
   package Angle_IO is new Ada.Text_IO.Float_IO (Hexapod.Legs.Angle);

   Scheduler : aliased Hexapod.Schedulers.Scheduler;
   Time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   Leg       : Hexapod.Legs.Leg;
   Angles    : Hexapod.Legs.Segment_Angles;
begin
   Leg.Configure
     (Segments => (30, 80, 110),
      Origin => (0, 0, 0),
      Rotated => 0.0,
      Scheduler => Scheduler'Unchecked_Access);

   Leg.Assign_Program
     (Program =>
        ((Tick   => 5.0,
          Target => (0, 50, -30),
          Linear => False),
         (Tick   => 6.0,
          Target => (0, 100, -50),
          Linear => True)),
      Repeat  => 2);

   for J in 1 .. 2 * (5 + 6) * 50 loop
      Scheduler.Call_Callbacks (Time);
      Time := Time + 0.02;

      delay until Time;
   end loop;

   Leg.Compute_Angles
     (Position => (X => 0, Y => 100, Z => -50),
      Angles   => Angles);

   Angle_IO.Put (Angles.S1);
   Angle_IO.Put (Angles.S2);
   Angle_IO.Put (Angles.S3);
end Hexapod.Run;
