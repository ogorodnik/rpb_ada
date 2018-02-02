
package body Sensors.Output.Led is

   --------
   -- On --
   --------

   procedure On (Self : in out Led) is
   begin
      Self.Pin.High;
   end On;

   ---------
   -- Off --
   ---------

   procedure Off (Self : in out Led) is
   begin
      Self.Pin.Low;
   end Off;

   ------------
   -- Create --
   ------------

   function Create (Num : GPIO.GPIO_Number) return Led is
   begin
      return Result : Led do
         Result.Node := new Output_Sensor_Node;
         Output_Sensor_Node_Access (Result.Node).Pin := GPIO.Create (Num);
      end return;
   end Create;

end Sensors.Output.Led;
