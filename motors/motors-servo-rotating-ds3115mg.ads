
---------------------
--  DS3115MG motor --
---------------------
--  Needs 50 MHz frequency on driver

package Motors.Servo.Rotating.DS3115MG is

   type DS3115MG_Motor is new Rotating_Servo_Motor with private;

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return DS3115MG_Motor;

private

   type DS3115MG_Motor is new Rotating_Servo_Motor with null record;

end Motors.Servo.Rotating.DS3115MG;
