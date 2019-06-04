
--------------------------------
--         SG90 motor         --
--  Rotates in range 0 .. 180 --
--------------------------------
--  Needs 50 MHz frequency on driver

package Motors.Servo.Angle.SG90 is

   type SG90_Motor is new Angle_Servo_Motor with private;

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return SG90_Motor;

private

   type SG90_Motor is new Angle_Servo_Motor with null record;

end Motors.Servo.Angle.SG90;
