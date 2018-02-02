
--  Tested with
--    Digital (Green) LED Light Module SKU:DFR0021-G

with GPIO;

package Sensors.Output.Led is

   type Led is tagged private;

   procedure On  (Self : in out Led);
   procedure Off (Self : in out Led);

   function Create (Num : GPIO.GPIO_Number) return Led;

private

   type Led is tagged record
      Pin : GPIO.Output_Pin;
   end record;

end Sensors.Output.Led;
