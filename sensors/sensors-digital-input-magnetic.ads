
-------------------------------------------
--  Tested with:                         --
--    DFRobot Digital Magnetic Sensor V2 --
-------------------------------------------

package Sensors.Digital.Input.Magnetic is

   type Magnetic is new Input_Sensor with private;

   function Is_Detected (Self : in out Magnetic) return Boolean;

   function Create (Num : GPIO.GPIO_Number) return Magnetic;

private

   type Magnetic is new Input_Sensor with null record;

end Sensors.Digital.Input.Magnetic;
