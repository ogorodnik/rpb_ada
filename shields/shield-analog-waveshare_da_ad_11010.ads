
--------------------------------------------------------
--  WAVESHARE
--  Raspberry Pi High-Precision AD/DA Expansion Board --
--  SKU: 11010
--
--  ADS 1255/1256
--
--  Uses SPI pins:
--    GPIO 7  - CE1
--    GPIO 8  - CE0
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
private with Interfaces;
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

   function Chip_Id (Self : DA_AD_Board) return Integer;

private

   type DA_AD_Board_Node is new Root_Shield_Node with record
      SPI   : GPIO.SPI.SPI;
      SPICS : GPIO.Output_Pin;
      DRDY  : GPIO.Input_Pin;
   end record;
   type DA_AD_Board_Node_Access is access all DA_AD_Board_Node;

   type DA_AD_Board is new Analog_Shield with null record;

   type Register_Offset is
     new GPIO.SPI.Unsigned_Integer_8 range 2#0000# .. 2#1111#;

   -- COMMANDS --

   type Command_Type is new GPIO.SPI.Unsigned_Integer_8;

   RDATA  : constant Command_Type := 2#00000001#;
   RREG   : constant Command_Type := 2#00010000#;
   WREG   : constant Command_Type := 2#01010000#;
   SYNC   : constant Command_Type := 2#11111100#;
   WAKEUP : constant Command_Type := 2#11111111#;

   function Get_Register
     (Self     : DA_AD_Board;
      Register : Register_Offset)
      return Interfaces.Unsigned_8;
   --  Read from register

   procedure Set_Register
     (Self     : DA_AD_Board;
      Register : Register_Offset;
      Data     : GPIO.SPI.Unsigned_Integer_8);
   --  Write to register

   procedure Send_Command
     (Self    : DA_AD_Board;
      Command : Command_Type);

   procedure ISP_Send
     (Self : DA_AD_Board;
      Data : GPIO.SPI.Unsigned_Integer_8);
   --  Sends via ISP with delay

   function Read_Data (Self : DA_AD_Board) return Long_Float;

end Shield.Analog.WAVESHARE_DA_AD_11010;
