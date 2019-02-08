
--------------------------------------------------------
--  Tested with                                       --
--    Digital (Green) LED Light Module SKU:DFR0021-G  --
--------------------------------------------------------

package Sensors.Digital.Output.Led is

   type Led is new Output_Sensor with private;

   procedure On  (Self : in out Led);
   procedure Off (Self : in out Led);

   function Create (Num : GPIO.GPIO_Number) return Led;

private

   type Led is new Output_Sensor with null record;

end Sensors.Digital.Output.Led;
