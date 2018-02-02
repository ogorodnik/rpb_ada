
package body Sensors.Output is

   --------
   -- Id --
   --------

   function Id (self : Output_Sensor) return GPIO.GPIO_Number is
   begin
      return Output_Sensor_Node_Access (Self.Node).Pin.Id;
   end Id;

   ----------
   -- High --
   ----------

   procedure High (self : Output_Sensor) is
   begin
      Output_Sensor_Node_Access (Self.Node).Pin.High;
   end High;

   ---------
   -- Low --
   ---------

   procedure Low (self : Output_Sensor) is
   begin
      Output_Sensor_Node_Access (Self.Node).Pin.Low;
   end low;

end Sensors.Output;
