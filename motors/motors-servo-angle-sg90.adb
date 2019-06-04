
-----------------
--  SG90 motor --
-----------------

with Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

package body Motors.Servo.Angle.SG90 is

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
         Result.Node := new Angle_Servo_Motor_Node'
           (Counter   => 1,
            Driver    => new Servo_Motor_Driver'Class' (Driver),
            Chanel    => Chanel,
            Min       => 0,
            Max       => 4096,
            Max_Angle => 180,
            MHz       => 50);

         Init (Result);

         if Driver in Servo_Driver_HAT'Class then
            Servo_Motor_Node_Access (Result.Node).Min := 650;
            Servo_Motor_Node_Access (Result.Node).Max := 2750;
         end if;
      end return;
   end Create;

end Motors.Servo.Angle.SG90;
