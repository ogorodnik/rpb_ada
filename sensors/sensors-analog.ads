
---------------------------------------------------------------------------
--  Base package for analog sensors                                      --
--  As far as Raspberry does not have any DA-AD convertors this kind of  --
--  sensors should be attached to some extension shield which has a AD   --
--  convertor.                                                           --
---------------------------------------------------------------------------

with Shield.Analog;

package Sensors.Analog is

   type Analog_Sensor is abstract new Sensor with private;

private

   type Analog_Sensor is abstract new Sensor with null record;

end Sensors.Analog;
