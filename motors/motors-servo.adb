
package body Motors.Servo is

   procedure Init (Self : Servo_Motor'Class) is
   begin
      if Servo_Motor_Driver_Access (Self.Node.Driver).Get_Frequency /=
        Servo_Motor_Node_Access (Self.Node).MHz
      then
         raise Program_Error
           with "The motor needs" &
           Servo_Motor_Node_Access (Self.Node).MHz'Img &
           " frequency but driver has" &
           Servo_Motor_Driver_Access (Self.Node.Driver).Get_Frequency'Img;
      end if;

      Do_Init (Self);
   end Init;

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
