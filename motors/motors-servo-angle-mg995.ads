
--------------------------------
--        MG995 motor         --
--  Rotates in range 0 .. 180 --
--------------------------------
--  Needs 50 Hz frequency on driver

package Motors.Servo.Angle.MG995 is

   type MG995_Motor is new Angle_Servo_Motor with private;

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return MG995_Motor;

private

   type MG995_Motor is new Angle_Servo_Motor with null record;

end Motors.Servo.Angle.MG995;
