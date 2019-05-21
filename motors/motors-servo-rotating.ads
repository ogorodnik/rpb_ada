
----------------------------------------------------------------
--  Base package for all servo motors with unlimited rotating --
----------------------------------------------------------------

package Motors.Servo.Rotating is

   type Direction_Type is (Up, Down);
   type Speed_Type is range 1 .. 10;

   type Rotating_Servo_Motor is abstract new Servo_Motor with private;

   function Get_Mid (Self : Rotating_Servo_Motor) return PWM_Value;
   procedure Set_Mid (Self : Rotating_Servo_Motor; Value : PWM_Value);

   procedure Rotate
     (Self      : Servo_Motor;
      Direction : Direction_Type;
      Speed     : Speed_Type);

   procedure Stop (Self : Servo_Motor);

private

   type Rotating_Servo_Motor_Node is new Servo_Motor_Node with record
      Mid : PWM_Value;
   end record;
   type Rotating_Servo_Motor_Node_Access is
     access all Rotating_Servo_Motor_Node;

   type Rotating_Servo_Motor is abstract new Servo_Motor with null record;

end Motors.Servo.Rotating;
