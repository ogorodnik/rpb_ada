
package Sensors.Analog.Input.Potentiometer is

   type Potentiometer is new Input_Analog_Sensor with private;

   function Create
     (Board : Shield.Analog.Analog_Shield_Access;
      Pin   : Shield.Analog.Input_Pin_Number)
      return Potentiometer;

   function Value
     (Self : Potentiometer)
      return Long_Float;

   procedure Set_Min
     (Self  : Potentiometer;
      Value : Long_Float);

   procedure Set_Max
     (Self  : Potentiometer;
      Value : Long_Float);

   function Level
     (Self : Potentiometer)
      return Level_Type;

private

   type Potentiometer_Node is new Input_Analog_Sensor_Node with record
      Min : Long_Float := 0.0;
      Max : Long_Float := 0.0;
   end record;
   type Potentiometer_Node_Access is access all Potentiometer_Node;

   type Potentiometer is new Input_Analog_Sensor with null record;

end Sensors.Analog.Input.Potentiometer;
