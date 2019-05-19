
-- This example shows how to drive servo motor.

with Ada.Text_IO;
with Ada.Exceptions;

with GPIO.I2C;
use GPIO.I2C;

with Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;
use  Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

with Motors.Servo.SG90;
use Motors.Servo.SG90;

procedure Test is
   --  Create I2C transport
   Protocol : I2C_BSC1 := Create;

   --  Create driver for extension shield connected to raspberry,
   --  with givven address and I2C as transport protocol
   Driver   : Servo_Driver_HAT := Create
     (Protocol, Address'(others => False));

   --  Create a srvo motor on Driver connected to first (0) pins
   Servo    : SG90_Motor := Create (Driver, 0);

begin
   --  rotate motor
   Servo.Set_Angle (0);
   delay 2.0;
   Servo.Set_Angle (90);
   delay 2.0;
   Servo.Set_Angle (180);

exception
   when E: others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Test;
