with Ada.Numerics;
private with Ada.Containers.Vectors;
private with Ada.Calendar;

with Hexapod.Programs;
with Hexapod.Schedulers;

with Motors.Servo.Angle.MG995;

package Hexapod.Legs is

   type Leg is limited new Hexapod.Schedulers.Listener with private;
   --  Hexapod leg representation

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

   type Motor_Array is array (1 .. 3) of Motors.Servo.Angle.MG995.MG995_Motor;

   not overriding procedure Configure
     (Self         : in out Leg;
      Segments     : Hexapod.Legs.Segments;
      Origin       : Position;
      Rotated      : Angle;
      Motors       : Motor_Array;
      Scheduler    : not null Hexapod.Schedulers.Scheduler_Access);
   --  Configure the leg - set segments' lengths and leg origin in the space.
   --  Origin is Position of the first leg segment.
   --  Rotated is an angle of the first leg segment in default position,
   --  zero means the segment is parallel to the X axe.
   --  Scheduler will be used monitor leg movement.

   not overriding procedure Assign_Program
     (Self    : in out Leg;
      Program : Hexapod.Programs.Program_Item_Array;
      Repeat  : Positive);
   --  Assign given program to the leg and start execution. Repeat the program
   --  given times.

   type Joint_Angles is record
      S1, S2, S3 : Angle range 0.0 .. π;  --  Angle of each segment of the leg
      --  S1 => A, S2 => B, S3 => C on the picture of back view
      --  Zero angle: S1 - X axes, S2 - Z axes (up), S3 - contr Z axes (down).
   end record;

   not overriding procedure Compute_Angles
     (Self     : Leg;
      Position : Hexapod.Position;
      Angles   : out Joint_Angles);
   --  Compute angles in segments for provided leg position.
   --  Position of leg end related to the leg origin (see Configure above).

private

   package Program_Item_Vectors is new Ada.Containers.Vectors
     (Positive, Hexapod.Programs.Program_Item, Hexapod.Programs."=");

   type Current_Program_Item is record
      Item     : Hexapod.Programs.Program_Item;
      Started  : Ada.Calendar.Time;
      Finished : Ada.Calendar.Time;
      Origin   : Position;
   end record;

   type Motor_Angles is
     array (Motor_Array'Range) of Motors.Servo.Angle.Angle_Type;

   type Leg is limited new Hexapod.Schedulers.Listener with record
      Segments     : Hexapod.Legs.Segments;
      Origin       : Hexapod.Position;
      Rotated      : Angle;
      Program      : Program_Item_Vectors.Vector;
      Item_Index   : Natural := 0;
      Repeat       : Natural := 0;
      Current_Item : Current_Program_Item;
      Joints       : Motor_Array;
      Joint_Angles : Motor_Angles := (others => 360);
      Scheduler    : Hexapod.Schedulers.Scheduler_Access;
   end record;

   not overriding procedure Next_Program_Item (Self : aliased in out Leg);
   --  Proceed to the next Program_Item in the programm

   not overriding procedure Rotate_Joints
     (Self   : in out Leg;
      Target : Position);
   --  Compute angles for given Position and send them to motors

   overriding procedure Callback
     (Self : in out Leg;
      Time : Ada.Calendar.Time);
   --  This callback is called by scheduler

end Hexapod.Legs;
