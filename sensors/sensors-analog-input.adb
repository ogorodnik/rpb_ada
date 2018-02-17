
package body Sensors.Analog.Input is

   ---------
   -- Get --
   ---------

   function Get (Self : Input_Analog_Sensor) return Long_Float
   is
      N : Input_Analog_Sensor_Node_Access :=
        Input_Analog_Sensor_Node_Access (Self.Node);

   begin
      return N.Board.Value (N.Pin);
   end Get;

   --------
   -- Id --
   --------

   function Id
     (Self : Input_Analog_Sensor)
      return Shield.Analog.Input_Pin_Number is
   begin
      return Input_Analog_Sensor_Node_Access (Self.Node).Pin;
   end Id;

end Sensors.Analog.Input;
