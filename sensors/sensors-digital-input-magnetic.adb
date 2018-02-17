
package body Sensors.Digital.Input.Magnetic is

   -----------------
   -- Is_Detected --
   -----------------

   function Is_Detected (Self : in out Magnetic) return Boolean is
   begin
      return Self.Is_High;
   end Is_Detected;

   ------------
   -- Create --
   ------------

   function Create (Num : GPIO.GPIO_Number) return Magnetic is
   begin
      return Result : Magnetic do
         Result.Node := new Input_Sensor_Node;
         Input_Sensor_Node_Access (Result.Node).Pin := GPIO.Create (Num);
      end return;
   end Create;

end Sensors.Digital.Input.Magnetic;
