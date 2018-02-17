
package Sensors.Digital.Input.IR is

   type IR is new Input_Sensor with private;

   function Is_High (Self : in out IR) return Boolean;

   function Create (Num : GPIO.GPIO_Number) return IR;

private

   type IR is new Input_Sensor with null record;

end Sensors.Digital.Input.IR;
