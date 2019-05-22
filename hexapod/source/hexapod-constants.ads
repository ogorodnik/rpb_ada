
with Shield.Motor_Drivers.Servo; use Shield.Motor_Drivers.Servo;
with Motors.Servo.Angle;         use Motors.Servo.Angle;

package Hexapod.Constants is

   type Leg_Number is range 1 .. 6;
   --  From forward-right
   --
   --         /- F -\
   --        6       1
   --        |       |
   --        5       2
   --         \     /
   --          4 - 3

   type Leg_Motor_Number is range 1 .. 3;
   --  From the motor which is the most close to body
   --               ^
   --              /3\
   --   --------  /   \
   --   Bdy. 1 - 2     \
   --   --------        \

   type Motor_PWM_Range is record
      Min : PWM_Value;
      Max : PWM_Value;
   end record;

   type Leg_PWM_Ranges is array (Leg_Motor_Number) of Motor_PWM_Range;

   type Leg_Start_Angles is array (Leg_Motor_Number) of Angle_Type;

   type Angles_Restrictions is record
      Min : Angle_Type;
      Max : Angle_Type;
   end record;

   type Leg_Angles_Restrictions is
     array (Leg_Motor_Number) of Angles_Restrictions;

   type Ordinats is (X, Y, Z);

   type Leg_Position is record
      X : Integer;
      Y : Integer;
      Z : Integer;
   end record;
   --  Position in milimeters


   ---------------
   -- Constants --
   ---------------

   Legs_PWM_Ranges : constant array (Leg_Number) of Leg_PWM_Ranges :=
     (1 =>
        (1 => (Min => 1250, Max => 2600),
         --  1250 is limitation of rotation by hardware,
         --  use 700 if Legs_Angles_Restrictions is in use
         2 => (Min => 700,  Max => 2400),
         3 => (Min => 650,  Max => 2400)),

      --  not set
      2 =>
        (1 => (Min => 0, Max => 0),
         2 => (Min => 0, Max => 0),
         3 => (Min => 0, Max => 0)),
      3 =>
        (1 => (Min => 0, Max => 0),
         2 => (Min => 0, Max => 0),
         3 => (Min => 0, Max => 0)),
      4 =>
        (1 => (Min => 0, Max => 0),
         2 => (Min => 0, Max => 0),
         3 => (Min => 0, Max => 0)),
      5 =>
        (1 => (Min => 0, Max => 0),
         2 => (Min => 0, Max => 0),
         3 => (Min => 0, Max => 0)),
      6 =>
        (1 => (Min => 0, Max => 0),
         2 => (Min => 0, Max => 0),
         3 => (Min => 0, Max => 0)));

   Legs_Start_Angles : constant array (Leg_Number) of Leg_Start_Angles :=
     (1 =>
        (1 => 180,
         2 => 0,
         3 => 0),

      --  not set
      2 =>
        (1 => 90,
         2 => 90,
         3 => 90),
      3 =>
        (1 => 90,
         2 => 90,
         3 => 90),
      4 =>
        (1 => 90,
         2 => 90,
         3 => 90),
      5 =>
        (1 => 90,
         2 => 90,
         3 => 90),
      6 =>
        (1 => 90,
         2 => 90,
         3 => 90));

   Legs_Angles_Restrictions : constant
     array (Leg_Number) of Leg_Angles_Restrictions :=
     (1 =>
        (1 => (Min => 53, Max => 180),
         2 => (Min => 0,  Max => 180),
         3 => (Min => 0,  Max => 180)),

      --  not set
      2 =>
        (1 => (Min => 0, Max => 180),
         2 => (Min => 0, Max => 180),
         3 => (Min => 0, Max => 180)),
      3 =>
        (1 => (Min => 0, Max => 180),
         2 => (Min => 0, Max => 180),
         3 => (Min => 0, Max => 180)),
      4 =>
        (1 => (Min => 0, Max => 180),
         2 => (Min => 0, Max => 180),
         3 => (Min => 0, Max => 180)),
      5 =>
        (1 => (Min => 0, Max => 180),
         2 => (Min => 0, Max => 180),
         3 => (Min => 0, Max => 180)),
      6 =>
        (1 => (Min => 0, Max => 180),
         2 => (Min => 0, Max => 180),
         3 => (Min => 0, Max => 180)));

   Legs_Start_Positions : constant
     array (Leg_Number) of Leg_Position :=
   --  Positions of legs when they are set in Legs_Start_Angles angles
   --  Ordinats start points are in the center of bearings of each leg
   --  Bearings are plased in the bottom of hexapod

     (1 =>
        (X => -9, Y => 46, Z => -28),

      --  not set
      2 =>
        (X => 0, Y => 0, Z => 0),
      3 =>
        (X => 0, Y => 0, Z => 0),
      4 =>
        (X => 0, Y => 0, Z => 0),
      5 =>
        (X => 0, Y => 0, Z => 0),
      6 =>
        (X => 0, Y => 0, Z => 0));

end Hexapod.Constants;
