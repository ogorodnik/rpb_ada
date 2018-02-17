
------------------------------------
--  Raspberry Pi3 ( bcm2835 ) SPI --
------------------------------------

--  GPIO Pins 7, 8, 9, 10, 11 are used for SPI

package GPIO.SPI is

   type Unsigned_Integer_8 is range 0 .. 2**8 - 1;
   for Unsigned_Integer_8'Size use 8;

   type Unsigned_Integer_32 is mod 2**32;
   for Unsigned_Integer_32'Size use 32;

   type SPI is tagged private;

   function Create return SPI;

   type SPI_Master_Mode is (Mode_A, Mode_B, Mode_C, Mode_D);
   --  Mode_A: CPHA = 0 CPOL = 0
   --  Mode_B: CPHA = 0 CPOL = 1
   --  Mode_C: CPHA = 1 CPOL = 0
   --  Mode_D: CPHA = 1 CPOL = 1

   procedure Set_Master_Mode
     (Self : SPI;
      Mode : SPI_Master_Mode);

   type SPI_Clock_Divider is
     (F_6_kHz, F_12_kHz, F_24_kHz, F_48_kHz, F_97_kHz, F_195_kHz, F_390_kHz,
      F_781_kHz, F_1_5_MHz, F_3_Mhz, F_6_MHz, F_12_MHz, F_25_Mhz, F_50_MHz,
      F_100_MHz, F_200_MHz);

   procedure Set_Clock_Divider
     (Self : SPI;
      Div  : SPI_Clock_Divider);

   procedure Send
     (Self : SPI;
      Data : Unsigned_Integer_32);

   function Get (Self : SPI) return Unsigned_Integer_32;

private

   type SPI_Node is record
      Counter : Natural := 0;
      CE1_N   : Alternate_Pin;
      CE0_N   : Alternate_Pin;
      MISO    : Alternate_Pin;
      MOSI    : Alternate_Pin;
      SCLK    : Alternate_Pin;
   end record;
   type SPI_Node_Access is access all SPI_Node;

   type SPI is new Ada.Finalization.Controlled with record
      Node : SPI_Node_Access;
   end record;

   overriding procedure Adjust   (Self : in out SPI);
   overriding procedure Finalize (Self : in out SPI);

end GPIO.SPI;
