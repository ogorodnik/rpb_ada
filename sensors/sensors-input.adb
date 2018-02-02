
package body Sensors.Input is

   --------
   -- Id --
   --------

   function Id (self : Input_Sensor) return GPIO.GPIO_Number is
   begin
      return Input_Sensor_Node_Access (Self.Node).Pin.Id;
   end Id;

   -------------
   -- Is_High --
   -------------

   function Is_High (self : Input_Sensor) return Boolean is
   begin
      return Input_Sensor_Node_Access (Self.Node).Pin.Is_High;
   end Is_High;

   ------------
   -- Is_Low --
   ------------

   function Is_Low (self : Input_Sensor) return Boolean is
   begin
      return Input_Sensor_Node_Access (Self.Node).Pin.Is_Low;
   end Is_low;

end Sensors.Input;
