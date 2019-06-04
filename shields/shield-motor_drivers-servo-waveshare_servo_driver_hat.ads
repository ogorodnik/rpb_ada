
--------------------------------------------------------
--  WAVESHARE
--  Servo Driver HAT for Raspberry Pi, 16-Channel, 12-bit, I2C Interface --
--  SKU: 15275
--
--------------------------------------------------------

with GPIO.I2C;

package Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT is

   type Servo_Driver_HAT is new Servo_Motor_Driver with private;

   type Addresses_Pins is (A0, A1, A2, A3, A4);
   --  On board selectable address

   type Address is array (Addresses_Pins) of Boolean;

   function Create
     (I2C          : GPIO.I2C.I2C_BSC1;
      Addr         : Address;
      Hz_Frequency : Frequency)
      return Servo_Driver_HAT;
   --  Hz_Frequency is an output frequency for motors

   overriding function Get_Frequency
     (Self : Servo_Driver_HAT)
      return Frequency;

   overriding procedure Rotate_Servo
     (Self   : Servo_Driver_HAT;
      Chanel : Motor_Chanel_Number;
      On     : PWM_Value;
      Off    : PWM_Value);

private

   type Servo_Driver_HAT_Node is new Root_Shield_Node with record
      I2C  : GPIO.I2C.I2C_BSC1;
      Addr : GPIO.I2C.Slave_Address_7Bit;
      Hz   : Frequency;
   end record;
   type Servo_Driver_HAT_Node_Access is access all Servo_Driver_HAT_Node;

   type Servo_Driver_HAT is new Servo_Motor_Driver with null record;


end Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;
