
with Ada.Unchecked_Deallocation;

package body Sensors.Analog.Input is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Shield.Analog.Analog_Shield'Class, Analog_Shield_Access);

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Input_Analog_Sensor) is
   begin
      Free (Input_Analog_Sensor_Node_Access (Self.Node).Board);
      Free (Analog_Sensor (Self));
   end Free;

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
