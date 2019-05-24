
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


   -- PWM_Range --
   --  For adjusting motor PWM min/max values if needed

   type Motor_PWM_Range is record
      Min : PWM_Value;
      Max : PWM_Value;
   end record;

   type Leg_PWM_Ranges is array (Leg_Motor_Number) of Motor_PWM_Range;

   -- Angles --
   --  restrictions in rotation because of hardware limitations

   type Motor_Angles is record
      Min : Angle_Type;
      Max : Angle_Type;
   end record;

   type Leg_Angles is array (Leg_Motor_Number) of Motor_Angles;

   ---------------
   -- Constants --
   ---------------

   Legs_PWM_Ranges : constant array (Leg_Number) of Leg_PWM_Ranges :=
     (1 =>
        (1 => (Min => 700, Max => 2400),
         2 => (Min => 700, Max => 2400),
         3 => (Min => 700, Max => 2400)),
      2 =>
        (1 => (Min => 700, Max => 2400),
         2 => (Min => 700, Max => 2400),
         3 => (Min => 700, Max => 2400)),
      3 =>
        (1 => (Min => 700, Max => 2400),
         2 => (Min => 700, Max => 2400),
         3 => (Min => 700, Max => 2400)),
      4 =>
        (1 => (Min => 700, Max => 2400),
         2 => (Min => 700, Max => 2400),
         3 => (Min => 700, Max => 2400)),
      5 =>
        (1 => (Min => 700, Max => 2400),
         2 => (Min => 700, Max => 2400),
         3 => (Min => 700, Max => 2400)),
      6 =>
        (1 => (Min => 700, Max => 2400),
         2 => (Min => 700, Max => 2400),
         3 => (Min => 700, Max => 2400)));

   Legs_Angles : constant
     array (Leg_Number) of Leg_Angles :=
     (1 =>
        (1 => (Min => 60, Max => 180),
         2 => (Min => 0,  Max => 180),
         3 => (Min => 0,  Max => 180)),

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
        (1 => (Min => 60, Max => 180),
         2 => (Min => 0, Max => 180),
         3 => (Min => 0, Max => 180)));

end Hexapod.Constants;
