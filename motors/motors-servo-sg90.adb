
-----------------
--  SG90 motor --
-----------------

with Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

package body Motors.Servo.SG90 is

   ------------
   -- Create --
   ------------

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return SG90_Motor
   is
      use Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;
   begin
      return Result : SG90_Motor do
         Result.Node := new SG90_Motor_Node'
           (Counter  => 1,
            Driver   => new Servo_Motor_Driver'Class' (Driver),
            Chanel   => Chanel,
            Min      => 0,
            Max      => 4096);

         if Driver in Servo_Driver_HAT'Class then
            SG90_Motor_Node_Access (Result.Node).Min := 650;
            SG90_Motor_Node_Access (Result.Node).Max := 2750;
         end if;
      end return;
   end Create;

   -------------
   -- Get_Min --
   -------------

   function Get_Min (Self : SG90_Motor) return PWM_Value is
   begin
      return SG90_Motor_Node_Access (Self.Node).Min;
   end Get_Min;

   -------------
   -- Get_Max --
   -------------

   function Get_Max (Self : SG90_Motor) return PWM_Value is
   begin
      return SG90_Motor_Node_Access (Self.Node).Max;
   end Get_Max;

   -------------
   -- Set_Min --
   -------------

   procedure Set_Min (Self : SG90_Motor; Value : PWM_Value) is
   begin
      SG90_Motor_Node_Access (Self.Node).Min := Value;
   end Set_Min;

   -------------
   -- Set_Max --
   -------------

   procedure Set_Max (Self : SG90_Motor; Value : PWM_Value) is
   begin
      SG90_Motor_Node_Access (Self.Node).Min := Value;
   end Set_Max;

   ---------------
   -- Set_Angle --
   ---------------

   overriding procedure Set_Angle
     (Self  : SG90_Motor;
      Angle : Angle_Type)
   is
      N : SG90_Motor_Node_Access := SG90_Motor_Node_Access (Self.Node);
      V : PWM_Value := PWM_Value
        (Integer (N.Min) +
         (Integer (N.Max) - Integer (N.Min)) / 180 * Integer (Angle));
   begin
      Servo_Motor_Driver_Access (N.Driver).Rotate_Servo (N.Chanel, 0, V);
   end Set_Angle;

end Motors.Servo.SG90;
