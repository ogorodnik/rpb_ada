
package body Sensors.Output.Buzzer is

   --------
   -- On --
   --------

   procedure On (Self : in out Buzzer) is
   begin
      Self.High;
   end On;

   ---------
   -- Off --
   ---------

   procedure Off (Self : in out Buzzer) is
   begin
      Self.Low;
   end Off;

   ------------
   -- Create --
   ------------

   function Create (Num : GPIO.GPIO_Number) return Buzzer is
   begin
      return Result : Buzzer do
         Result.Node := new Output_Sensor_Node;
         Output_Sensor_Node_Access (Result.Node).Pin := GPIO.Create (Num);
      end return;
   end Create;

end Sensors.Output.Buzzer;
