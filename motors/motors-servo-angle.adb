
package body Motors.Servo.Angle is

   ---------------
   -- Set_Angle --
   ---------------

   procedure Set_Angle
     (Self  : Angle_Servo_Motor;
      Angle : Angle_Type)
   is
      N : Angle_Servo_Motor_Node_Access :=
        Angle_Servo_Motor_Node_Access (Self.Node);
      V : PWM_Value;
   begin
      if Angle > N.Max_Angle then
         raise Constraint_Error with
           "Angle should be in the range 0 .." & N.Max_Angle'Img;
      end if;

      V := PWM_Value
        (Integer (N.Min) +
         (Integer (N.Max) - Integer (N.Min)) /
           Integer (N.Max_Angle) * Integer (Angle));

      Servo_Motor_Driver_Access (N.Driver).Rotate_Servo (N.Chanel, 0, V);
   end Set_Angle;

end Motors.Servo.Angle;
