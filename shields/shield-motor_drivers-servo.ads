
-----------------------------------------------
--  Base package for all servo motor drivers --
-----------------------------------------------

package Shield.Motor_Drivers.Servo is

   type PWM_Value is new Natural;

   type Servo_Motor_Driver is abstract new Motor_Driver with private;

   procedure Rotate_Servo
     (Self   : Servo_Motor_Driver;
      Chanel : Motor_Chanel_Number;
      On     : PWM_Value;
      Off    : PWM_Value) is abstract;

private

   type Servo_Motor_Driver is abstract new Motor_Driver with null record;

end Shield.Motor_Drivers.Servo;
