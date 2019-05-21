
package body Motors.Servo.Rotating is

   -------------
   -- Get_Mid --
   -------------

   function Get_Mid (Self : Rotating_Servo_Motor) return PWM_Value is
   begin
      return Rotating_Servo_Motor_Node_Access (Self.Node).Mid;
   end Get_Mid;

   -------------
   -- Set_Mid --
   -------------

   procedure Set_Mid (Self : Rotating_Servo_Motor; Value : PWM_Value) is
   begin
      Rotating_Servo_Motor_Node_Access (Self.Node).Mid := Value;
   end Set_Mid;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : Servo_Motor) is
   begin
      Servo_Motor_Driver_Access (Self.Node.Driver).Rotate_Servo
        (Servo_Motor_Node_Access (Self.Node).Chanel,
         0,
         Rotating_Servo_Motor_Node_Access (Self.Node).Mid);
   end Stop;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (Self      : Servo_Motor;
      Direction : Direction_Type;
      Speed     : Speed_Type)
   is
      N : Rotating_Servo_Motor_Node_Access :=
        Rotating_Servo_Motor_Node_Access (Self.Node);
      V : PWM_Value;
   begin
      if Direction = Down then
         V := PWM_Value
           (Integer (N.Min) +
            (Integer (N.Mid) - Integer (N.Min)) / 10 * Integer (Speed));
      else
         V := PWM_Value
           (Integer (N.Mid) +
            (Integer (N.Max) - Integer (N.Mid)) / 10 * Integer (Speed));
      end if;

      Servo_Motor_Driver_Access (N.Driver).Rotate_Servo (N.Chanel, 0, V);
   end Rotate;

end Motors.Servo.Rotating;
