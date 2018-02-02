
package Sensors.Input is

   type Input_Sensor is abstract new Sensor with private;

   function Id (self : Input_Sensor) return GPIO.GPIO_Number;

private

   type Input_Sensor_Node is new Sensor_Node with record
      Pin : GPIO.Input_Pin;
   end record;
   type Input_Sensor_Node_Access is access all Input_Sensor_Node;

   type Input_Sensor is abstract new Sensor with null record;

   function Is_High (self : Input_Sensor) return Boolean;
   function Is_Low (self : Input_Sensor) return Boolean;

end Sensors.Input;
