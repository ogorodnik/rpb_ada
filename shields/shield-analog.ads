
------------------------------------------
--  Base package for all analog shields --
------------------------------------------

package Shield.Analog is

   type Input_Pin_Number is new Natural;

   type Analog_Shield is abstract new Root_Shield with private;

   function Value
     (Self  : Analog_Shield;
      Pin   : Input_Pin_Number)
      return Long_Float is abstract;

private

   type Analog_Shield is abstract new Root_Shield with null record;

end Shield.Analog;
