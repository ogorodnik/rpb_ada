
----------------------------------------
--  Base package for all servo motors --
----------------------------------------

package Motors.Servo is

   type Angle_Type is range 0 .. 180;

   type Servo_Motor is abstract new Motor with private;

   procedure Set_Angle
     (Self  : Servo_Motor;
      Angle : Angle_Type) is abstract;

private

   type Servo_Motor is abstract new Motor with null record;

end Motors.Servo;
