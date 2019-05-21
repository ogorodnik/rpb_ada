with Hexapod.Legs;
with Hexapod.Programs;
with Hexapod.Schedulers;

with Ada.Calendar;
with Ada.Text_IO;

procedure Hexapod.Run is
   use all type Hexapod.Legs.Angle;
   use type Ada.Calendar.Time;
   package Angle_IO is new Ada.Text_IO.Float_IO (Hexapod.Legs.Angle);

   Scheduler : aliased Hexapod.Schedulers.Scheduler;
   Time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   Leg       : Hexapod.Legs.Leg;
   Angles    : Hexapod.Legs.Segment_Angles;
begin
   Leg.Configure
     (Segments => (30, 85, 120),
      Origin => (0, 0, 0),
      Rotated => 0.0,
      Scheduler => Scheduler'Unchecked_Access);

   Leg.Compute_Angles
     (Position => (X => 40, Y => 69, Z => -30),
      Angles   => Angles);

   Angle_IO.Put (Angles.S1);
   Angle_IO.Put (Angles.S2);
   Angle_IO.Put (Angles.S3);

   --  Move the leg to initial position
   Leg.Assign_Program
     (Program =>
        (1 =>
             (Tick   => 1.0,
              Target => (40, 69, -50),
              Linear => False)),
      Repeat  => 1);

   Scheduler.Call_Callbacks (Time);
   Time := Time + 1.0;

   delay until Time;

   --  Take two steps
   Leg.Assign_Program
     (Program =>
        Hexapod.Programs.Create_Step
          (From   => (40, 69, -50),
           To     => (-40, 69, -50),
           Height => 20,
           Up     => 1.0,
           Down   => 4.0),
      Repeat  => 2);

   for J in 1 .. 2 * 5 * 10 loop
      Scheduler.Call_Callbacks (Time);
      Time := Time + 0.10;

      delay until Time;
   end loop;

end Hexapod.Run;
