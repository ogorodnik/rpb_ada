
package body Sensors.Digital.Input.IR is

   -------------
   -- Is_High --
   -------------

   function Is_High (Self : in out IR) return Boolean is
   begin
      return Input_Sensor (Self).Is_High;
   end Is_High;

   ------------
   -- Create --
   ------------

   function Create (Num : GPIO.GPIO_Number) return IR is
   begin
      return Result : IR do
         Result.Node := new Input_Sensor_Node;
         Input_Sensor_Node_Access (Result.Node).Pin := GPIO.Create (Num);
      end return;
   end Create;

end Sensors.Digital.Input.IR;
