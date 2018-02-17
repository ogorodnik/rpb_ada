
--------------------------------------------------------
--  WAVESHARE
--  Raspberry Pi High-Precision AD/DA Expansion Board --
--  SKU: 11010
--
--  ADS 1255/1256
--
--  Uses SPI pins:
--    GPIO 9  - MISO
--    GPIO 10 - MOSI
--    GPIO 11 - SCLK
--
--  Uses additional GPIO pins:
--    GPIO 17 - DRDY
--    GPIO 18 - RESET
--    GPIO 22 - CS0
--    GPIO 23 - CS1
--    GPIO 27 - PDWN
--
--  Single-Ended Inputs
--------------------------------------------------------

private with Ada.Finalization;
private with GPIO.SPI;

package Shield.Analog.WAVESHARE_DA_AD_11010 is

   type DA_AD_Board is new Analog_Shield with private;

   type Board_Mode is (DA, AD);
   --  DA mode is not implemented yet

   subtype AD_Input_Number is Input_Pin_Number range 0 .. 7;

   function Create (Mode : Board_Mode) return DA_AD_Board;

   overriding function Value
     (Self : DA_AD_Board;
      Pin  : Input_Pin_Number)
      return Long_Float;

private

   type DA_AD_Board_Node is new Root_Shield_Node with record
      SPI   : GPIO.SPI.SPI;
      SPICS : GPIO.Output_Pin;
      DRDY  : GPIO.Input_Pin;
   end record;
   type DA_AD_Board_Node_Access is access all DA_AD_Board_Node;

   type DA_AD_Board is new Analog_Shield with null record;

end Shield.Analog.WAVESHARE_DA_AD_11010;
