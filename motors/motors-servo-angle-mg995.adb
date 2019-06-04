
with Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

package body Motors.Servo.Angle.MG995 is

   ------------
   -- Create --
   ------------

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return MG995_Motor
   is
      use Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;
   begin
      return Result : MG995_Motor do
         Result.Node := new Angle_Servo_Motor_Node'
           (Counter   => 1,
            Driver    => new Servo_Motor_Driver'Class' (Driver),
            Chanel    => Chanel,
            Min       => 700,
            Max       => 2400,
            Max_Angle => 180,
            MHz_Min   => 50,
            MHz_Max   => 50);

         Check_Driver_Settings (Result);

         if Driver in Servo_Driver_HAT'Class then
            Servo_Motor_Node_Access (Result.Node).Min := 400;
            Servo_Motor_Node_Access (Result.Node).Max := 2200;
         end if;
      end return;
   end Create;

end Motors.Servo.Angle.MG995;
