
package body Motors.Servo.Rotating.DS3115MG is

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return DS3115MG_Motor is
   begin
      return Result : DS3115MG_Motor do
         Result.Node := new Rotating_Servo_Motor_Node'
           (Counter  => 1,
            Driver   => new Servo_Motor_Driver'Class' (Driver),
            Chanel   => Chanel,
            Min      => 1000,
            Mid      => 1500,
            Max      => 2000,
            MHz      => 50);

         Init (Result);
      end return;
   end Create;

end Motors.Servo.Rotating.DS3115MG;
