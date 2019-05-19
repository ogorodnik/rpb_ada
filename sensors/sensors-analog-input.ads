
--------------------------------------------
--  Base package for analog input sensors --
--------------------------------------------

package Sensors.Analog.Input is

   type Level_Type is range 0 .. 100;

   type Input_Analog_Sensor is abstract new Analog_Sensor with private;

   function Id
     (Self : Input_Analog_Sensor)
      return Shield.Analog.Input_Pin_Number;

private

   type Analog_Shield_Access is access all Shield.Analog.Analog_Shield'Class;

   type Input_Analog_Sensor_Node is new Sensor_Node with record
      Pin   : Shield.Analog.Input_Pin_Number;
      Board : Analog_Shield_Access;
   end record;
   type Input_Analog_Sensor_Node_Access is access all Input_Analog_Sensor_Node;

   type Input_Analog_Sensor is abstract new Analog_Sensor with null record;

   overriding procedure Free (Self : in out Input_Analog_Sensor);

   function Get (Self : Input_Analog_Sensor) return Long_Float;

end Sensors.Analog.Input;
