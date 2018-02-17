
package Sensors.Digital.Output is

   type Output_Sensor is abstract new Digital_Sensor with private;

   function Id (Self : Output_Sensor) return GPIO.GPIO_Number;

private

   type Output_Sensor_Node is new Sensor_Node with record
      Pin : GPIO.Output_Pin;
   end record;
   type Output_Sensor_Node_Access is access all Output_Sensor_Node;

   type Output_Sensor is abstract new Digital_Sensor with null record;

   procedure High (self : Output_Sensor);
   procedure Low (self : Output_Sensor);

end Sensors.Digital.Output;
