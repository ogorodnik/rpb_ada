
with Shield.Analog;

package Sensors.Analog is

   type Analog_Sensor is abstract new Sensor with private;

private

   type Analog_Sensor is abstract new Sensor with null record;

end Sensors.Analog;
