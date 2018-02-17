
package body Sensors.Digital.Input.Button is

   ----------------
   -- Is_Pressed --
   ----------------

   function Is_Pressed (Self : in out Button) return Boolean is
   begin
      return Self.Is_High;
   end Is_Pressed;

   ------------
   -- Create --
   ------------

   function Create (Num : GPIO.GPIO_Number) return Button is
   begin
      return Result : Button do
         Result.Node := new Input_Sensor_Node;
         Input_Sensor_Node_Access (Result.Node).Pin := GPIO.Create (Num);
      end return;
   end Create;

end Sensors.Digital.Input.Button;
