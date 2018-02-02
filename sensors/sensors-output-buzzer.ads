
-- Tested with:
--    Digital Buzzer For Arduino SKU:DFR0032

with GPIO;

package Sensors.Output.Buzzer is

   type Buzzer is new Output_Sensor with private;

   procedure On  (Self : in out Buzzer);
   procedure Off (Self : in out Buzzer);

   function Create (Num : GPIO.GPIO_Number) return Buzzer;

private

   type Buzzer is new Output_Sensor with null record;

end Sensors.Output.Buzzer;
