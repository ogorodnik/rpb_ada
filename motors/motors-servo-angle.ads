
--------------------------------------------------------------
--  Base package for all servo motors with limited rotating --
--------------------------------------------------------------

package Motors.Servo.Angle is

   type Angle_Type is range 0 .. 360;

   type Angle_Servo_Motor is abstract new Servo_Motor with private;

   procedure Set_Angle
     (Self  : Angle_Servo_Motor;
      Angle : Angle_Type);

private

   type Angle_Servo_Motor_Node is new Servo_Motor_Node with record
      Max_Angle : Angle_Type;
   end record;
   type Angle_Servo_Motor_Node_Access is
     access all Angle_Servo_Motor_Node;

   type Angle_Servo_Motor is abstract new Servo_Motor with null record;

end Motors.Servo.Angle;
