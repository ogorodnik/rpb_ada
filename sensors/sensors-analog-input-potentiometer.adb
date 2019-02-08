
package body Sensors.Analog.Input.Potentiometer is

   ------------
   -- Create --
   ------------

   function Create
     (Board : Shield.Analog.Analog_Shield_Access;
      Pin   : Shield.Analog.Input_Pin_Number)
      return Potentiometer is
   begin
      return Result : Potentiometer do
         Result.Node := new Potentiometer_Node'
           (Counter => 1,
            Pin     => Pin,
            Board   => Board,
            others => <>);
      end return;
   end Create;

   -----------
   -- Value --
   -----------

   function Value
     (Self : Potentiometer)
      return Long_Float is
   begin
      return Self.Get;
   end Value;

   -------------
   -- Set_Min --
   -------------

   procedure Set_Min
     (Self  : Potentiometer;
      Value : Long_Float) is
   begin
      Potentiometer_Node_Access (Self.Node).Min := Value;
   end Set_Min;

   -------------
   -- Set_Max --
   -------------

   procedure Set_Max
     (Self  : Potentiometer;
      Value : Long_Float) is
   begin
      Potentiometer_Node_Access (Self.Node).Max := Value;
   end Set_Max;

   -----------
   -- Level --
   -----------

   function Level
     (Self : Potentiometer)
      return Level_Type
   is
      N   : Potentiometer_Node_Access := Potentiometer_Node_Access (Self.Node);
      One : Long_Float;
   begin
      if N.Max = 0.0 then
         return 0;
      end if;

      One := (N.Max - N.Min) / 100.0;

      return Level_Type (Self.Get / One);
   end Level;

end Sensors.Analog.Input.Potentiometer;
