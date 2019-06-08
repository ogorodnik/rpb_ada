
with Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

package body Motors.Servo.Angle.DS3218MG is

   ------------
   -- Create --
   ------------

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number;
      Angle  : DS3218MG_Angle)
      return DS3218MG_Motor
   is
      use Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;
   begin
      return Result : DS3218MG_Motor do
         Result.Node := new Angle_Servo_Motor_Node'
           (Counter   => 1,
            Driver    => new Servo_Motor_Driver'Class' (Driver),
            Chanel    => Chanel,
            Min       => 500,
            Max       => 2500,
            Max_Angle => (if Angle = 180 then 180 else 270),
            MHz_Min   => 50,
            MHz_Max   => 50);

         Check_Driver_Settings (Result);

         if Driver in Servo_Driver_HAT'Class then
            Servo_Motor_Node_Access (Result.Node).Min := 500;
            Servo_Motor_Node_Access (Result.Node).Max := 2500;
         end if;
      end return;
   end Create;

end Motors.Servo.Angle.DS3218MG;
