
package body Motors.Servo is

   -------------
   -- Get_Min --
   -------------

   function Get_Min (Self : Servo_Motor) return PWM_Value is
   begin
      return Servo_Motor_Node_Access (Self.Node).Min;
   end Get_Min;

   -------------
   -- Get_Max --
   -------------

   function Get_Max (Self : Servo_Motor) return PWM_Value is
   begin
      return Servo_Motor_Node_Access (Self.Node).Max;
   end Get_Max;

   -------------
   -- Set_Min --
   -------------

   procedure Set_Min (Self : Servo_Motor; Value : PWM_Value) is
   begin
      Servo_Motor_Node_Access (Self.Node).Min := Value;
   end Set_Min;

   -------------
   -- Set_Max --
   -------------

   procedure Set_Max (Self : Servo_Motor; Value : PWM_Value) is
   begin
      Servo_Motor_Node_Access (Self.Node).Min := Value;
   end Set_Max;

end Motors.Servo;
