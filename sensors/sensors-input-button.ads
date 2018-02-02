
--  Tested with:
--    DFRobot Digital Push Button SKU:DFR0029

package Sensors.Input.Button is

   type Button is new Input_Sensor with private;

   function Is_Pressed (Self : in out Button) return Boolean;

   function Create (Num : GPIO.GPIO_Number) return Button;

private

   type Button is new Input_Sensor with null record;

end Sensors.Input.Button;
