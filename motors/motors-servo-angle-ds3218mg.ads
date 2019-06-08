
------------------------------------
--          DS3218MG motor        --
--  Rotates in range 0 .. 180/270 --
------------------------------------
--  Needs 50 Hz frequency on driver

package Motors.Servo.Angle.DS3218MG is

   type DS3218MG_Angle is (180, 270);

   type DS3218MG_Motor is new Angle_Servo_Motor with private;

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number;
      Angle  : DS3218MG_Angle)
      return DS3218MG_Motor;

private

   type MG995_Motor is new Angle_Servo_Motor with null record;

end Motors.Servo.Angle.DS3218MG;
