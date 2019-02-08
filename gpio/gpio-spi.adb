
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;    use System, System.Storage_Elements;
with Interfaces.C;               use Interfaces.C;

with Peripherals;

package body GPIO.SPI is

   SPI0_Base_Address : constant Address :=
     Peripherals.Base_Address + 16#20_4000#;

   --  SPI0_CS --

   SPI0_CS_Address : constant Address := SPI0_Base_Address;

   type SPI0_CS_Chip_Select is (Select_0, Select_1, Select_2);
   for SPI0_CS_Chip_Select use (Select_0 => 0, Select_1 => 1, Select_2 => 2);
   for SPI0_CS_Chip_Select'Size use 2;

   type SPI0_CS_Clock_Phase_And_Polarity is
     (Middle_Low, Beginning_Low, Middle_High, Beginning_High);
   for SPI0_CS_Clock_Phase_And_Polarity use
     (Middle_Low  => 2#00#, Beginning_Low  => 2#01#,
      Middle_High => 2#10#, Beginning_High => 2#11#);
   for SPI0_CS_Clock_Phase_And_Polarity'Size use 2;

   type SPI0_CS_CLEAR_FIFO_Clear is (No, TX, RX, Both);
   for SPI0_CS_CLEAR_FIFO_Clear use (No => 0, TX => 1, RX => 2, Both => 3);
   for SPI0_CS_CLEAR_FIFO_Clear'Size use 2;

   type SPI0_CS_Polarity is (Low, High);
   for SPI0_CS_Polarity use (Low => 0, High => 1);
   for SPI0_CS_Polarity'Size use 1;

   type SPI0_CS_Chip_Select_Polarity is new SPI0_CS_Polarity;

   type SPI0_CS_Transfer_Active is (Not_Active, Active);
   for SPI0_CS_Transfer_Active use (Not_Active => 0, Active => 1);
   for SPI0_CS_Transfer_Active'Size use 1;

   type SPI0_CS_DMAEN_DMA_Enable is (No_DMA, Enable_DMA);
   for SPI0_CS_DMAEN_DMA_Enable use (No_DMA => 0, Enable_DMA => 1);
   for SPI0_CS_DMAEN_DMA_Enable'Size use 1;

   type SPI0_CS_Interrupt is (Do_Not_Generate, Generate);
   for SPI0_CS_Interrupt use (Do_Not_Generate => 0, Generate => 1);
   for SPI0_CS_Interrupt'Size use 1;

   type SPI0_CS_INTD_Interrupt_on_Done is new SPI0_CS_Interrupt;

   type SPI0_CS_INTR_Interrupt_on_RXR is new SPI0_CS_Interrupt;

   type SPI0_CS_ADSC_Automatically_Deassert_Chip_Select is
     (Do_Not_Auto_Deassert, Auto_Deassert);
   for SPI0_CS_ADSC_Automatically_Deassert_Chip_Select use
     (Do_Not_Auto_Deassert => 0, Auto_Deassert => 1);
   for SPI0_CS_ADSC_Automatically_Deassert_Chip_Select'Size use 1;

   type SPI0_CS_REN_Read_Enable is (Write, Read);
   for SPI0_CS_REN_Read_Enable use (Write => 0, Read => 1);
   for SPI0_CS_REN_Read_Enable'Size use 1;

   type SPI0_CS_LEN_LoSSI_Enable is (SPI_Master, LoSSI_Master);
   for SPI0_CS_LEN_LoSSI_Enable use (SPI_Master => 0, LoSSI_Master => 1);
   for SPI0_CS_LEN_LoSSI_Enable'Size use 1;

   type SPI0_CS_Unused is (Zero);
   for SPI0_CS_Unused use (Zero => 0);
   for SPI0_CS_Unused'Size use 1;

   type SPI0_CS_DONE_Transfer_Done is (In_Progress, Complete);
   for SPI0_CS_DONE_Transfer_Done use (In_Progress => 0, Complete => 1);
   for SPI0_CS_DONE_Transfer_Done'Size use 1;

   type SPI0_CS_RXD_RX_FIFO_Contains_Data is (Is_Empty, Contains_Data);
   for SPI0_CS_RXD_RX_FIFO_Contains_Data use
     (Is_Empty => 0, Contains_Data => 1);
   for SPI0_CS_RXD_RX_FIFO_Contains_Data'Size use 1;

   type SPI0_CS_TXD_TX_FIFO_Can_Accept_Data is (Is_Full, Has_Space);
   for SPI0_CS_TXD_TX_FIFO_Can_Accept_Data use (Is_Full => 0, Has_Space => 1);
   for SPI0_CS_TXD_TX_FIFO_Can_Accept_Data'Size use 1;

   type SPI0_CS_RXR_RX_FIFO_Needs_Reading is (Less_Than_Full, Is_Or_More_Full);
   for SPI0_CS_RXR_RX_FIFO_Needs_Reading use
     (Less_Than_Full => 0, Is_Or_More_Full => 1);
   for SPI0_CS_RXR_RX_FIFO_Needs_Reading'Size use 1;

   type SPI0_CS_RXF_RX_FIFO_Full is (Not_Full, Full);
   for SPI0_CS_RXF_RX_FIFO_Full use (Not_Full => 0, Full => 1);
   for SPI0_CS_RXF_RX_FIFO_Full'Size use 1;

   type SPI0_CS_Chip_Select_0_Polarity is new SPI0_CS_Polarity;
   type SPI0_CS_Chip_Select_1_Polarity is new SPI0_CS_Polarity;
   type SPI0_CS_Chip_Select_2_Polarity is new SPI0_CS_Polarity;

   type SPI0_CS_Enable_DMA_mode_in_Lossi_mode is (Disable, Enable);
   for SPI0_CS_Enable_DMA_mode_in_Lossi_mode use (Disable => 0, Enable => 1);
   for SPI0_CS_Enable_DMA_mode_in_Lossi_mode'Size use 1;

   type SPI0_CS_Enable_Long_data_word_in_Lossi_mode is (Byte, Word);
   for SPI0_CS_Enable_Long_data_word_in_Lossi_mode use (Byte => 0, Word => 1);
   for SPI0_CS_Enable_Long_data_word_in_Lossi_mode'Size use 1;

   type SPI0_CS_Reserved is (Reserved);
   for SPI0_CS_Reserved use (Reserved => 0);
   for SPI0_CS_Reserved'Size use 6;

   type SPI0_CS_Register_Type is record
      CS       : SPI0_CS_Chip_Select;
      CPHA_OL  : SPI0_CS_Clock_Phase_And_Polarity;
      CLEAR    : SPI0_CS_CLEAR_FIFO_Clear;
      CSPOL    : SPI0_CS_Chip_Select_Polarity;
      TA       : SPI0_CS_Transfer_Active;
      DMAEN    : SPI0_CS_DMAEN_DMA_Enable;
      INTD     : SPI0_CS_INTD_Interrupt_on_Done;
      INTR     : SPI0_CS_INTR_Interrupt_on_RXR;
      ADSC     : SPI0_CS_ADSC_Automatically_Deassert_Chip_Select;
      REN      : SPI0_CS_REN_Read_Enable;
      LEN      : SPI0_CS_LEN_LoSSI_Enable;
      LMONO    : SPI0_CS_Unused;
      TE_EN    : SPI0_CS_Unused;
      DONE     : SPI0_CS_DONE_Transfer_Done;
      RXD      : SPI0_CS_RXD_RX_FIFO_Contains_Data;
      TXD      : SPI0_CS_TXD_TX_FIFO_Can_Accept_Data;
      RXR      : SPI0_CS_RXR_RX_FIFO_Needs_Reading;
      RXF      : SPI0_CS_RXF_RX_FIFO_Full;
      CSPOL0   : SPI0_CS_Chip_Select_0_Polarity;
      CSPOL1   : SPI0_CS_Chip_Select_1_Polarity;
      CSPOL2   : SPI0_CS_Chip_Select_2_Polarity;
      DMA_LEN  : SPI0_CS_Enable_DMA_mode_in_Lossi_mode;
      LEN_LONG : SPI0_CS_Enable_Long_data_word_in_Lossi_mode;
      RESERVED : SPI0_CS_Reserved;
   end record;
   for SPI0_CS_Register_Type use record
      CS       at 0 range  0 .. 1;
      CPHA_OL  at 0 range  2 .. 3;
      CLEAR    at 0 range  4 .. 5;
      CSPOL    at 0 range  6 .. 6;
      TA       at 0 range  7 .. 7;
      DMAEN    at 0 range  8 .. 8;
      INTD     at 0 range  9 .. 9;
      INTR     at 0 range 10 .. 10;
      ADSC     at 0 range 11 .. 11;
      REN      at 0 range 12 .. 12;
      LEN      at 0 range 13 .. 13;
      LMONO    at 0 range 14 .. 14;
      TE_EN    at 0 range 15 .. 15;
      DONE     at 0 range 16 .. 16;
      RXD      at 0 range 17 .. 17;
      TXD      at 0 range 18 .. 18;
      RXR      at 0 range 19 .. 19;
      RXF      at 0 range 20 .. 20;
      CSPOL0   at 0 range 21 .. 21;
      CSPOL1   at 0 range 22 .. 22;
      CSPOL2   at 0 range 23 .. 23;
      DMA_LEN  at 0 range 24 .. 24;
      LEN_LONG at 0 range 25 .. 25;
      RESERVED at 0 range 26 .. 31;
   end record;
   for SPI0_CS_Register_Type'Size use 32;

   SPI0_CS : SPI0_CS_Register_Type;
   for SPI0_CS'Address use SPI0_CS_Address;
   pragma Volatile (SPI0_CS);
   pragma Atomic (SPI0_CS);

   -- SPI0_FIFO --

   SPI0_FIFO_Address : constant Address := SPI0_Base_Address + 16#4#;

   type SPI0_FIFO_Register_Type is range 0 .. 2**32 - 1;
   for SPI0_FIFO_Register_Type'Size use 32;

   SPI0_FIFO : SPI0_FIFO_Register_Type;
   for SPI0_FIFO'Address use SPI0_FIFO_Address;
   pragma Volatile (SPI0_FIFO);
   pragma Atomic (SPI0_FIFO);

   --  SPI0_CLK --

   SPI0_CLK_Address : constant Address := SPI0_Base_Address + 16#8#;

   type SPI0_CLK_CDIV_Clock_Divider is range 0 .. 2**16 - 1;
   for SPI0_CLK_CDIV_Clock_Divider'Size use 16;

   type SPI0_CLK_Reserved is (Reserved);
   for SPI0_CLK_Reserved use (Reserved => 0);
   for SPI0_CLK_Reserved'Size use 16;

   type SPI0_CLK_Register_Type is record
      CDIV     : SPI0_CLK_CDIV_Clock_Divider;
      RESERVED : SPI0_CLK_Reserved;
   end record;
   for SPI0_CLK_Register_Type use record
      CDIV     at 0 range   0 .. 15;
      RESERVED at 0 range  16 .. 31;
   end record;
   for SPI0_CLK_Register_Type'Size use 32;

   SPI0_CLK : SPI0_CLK_Register_Type;
   for SPI0_CLK'Address use SPI0_CLK_Address;
   pragma Volatile (SPI0_CLK);
   pragma Atomic (SPI0_CLK);

   -- Internal --

   procedure Free is
     new Ada.Unchecked_Deallocation (SPI_Node, SPI_Node_Access);

   ------------
   -- Create --
   ------------

   function Create return SPI is
      CE1_N : Alternate_Pin;
      CE0_N : Alternate_Pin;
      MISO  : Alternate_Pin;
      MOSI  : Alternate_Pin;
      SCLK  : Alternate_Pin;

   begin
      CE1_N := Create (7,  Alt_0);
      CE0_N := Create (8,  Alt_0);
      MISO  := Create (9,  Alt_0);
      MOSI  := Create (10, Alt_0);
      SCLK  := Create (11, Alt_0);

      --  Clear all
      SPI0_CS :=
        (CS       => Select_0,
         CPHA_OL  => Middle_Low,
         CLEAR    => No,
         CSPOL    => Low,
         TA       => Not_Active,
         DMAEN    => No_DMA,
         INTD     => Do_Not_Generate,
         INTR     => Do_Not_Generate,
         ADSC     => Do_Not_Auto_Deassert,
         REN      => Write,
         LEN      => SPI_Master,
         LMONO    => Zero,
         TE_EN    => Zero,
         DONE     => In_Progress,
         RXD      => Is_Empty,
         TXD      => Is_Full,
         RXR      => Less_Than_Full,
         RXF      => Not_Full,
         CSPOL0   => Low,
         CSPOL1   => Low,
         CSPOL2   => Low,
         DMA_LEN  => Disable,
         LEN_LONG => Byte,
         RESERVED => Reserved);

      --  Clear both TX and RX
--        SPI0_CS.CLEAR := Both;

      SPI0_CS :=
        (CS       => Select_0,
         CPHA_OL  => Middle_Low,
         CLEAR    => Both,
         CSPOL    => Low,
         TA       => Not_Active,
         DMAEN    => No_DMA,
         INTD     => Do_Not_Generate,
         INTR     => Do_Not_Generate,
         ADSC     => Do_Not_Auto_Deassert,
         REN      => Write,
         LEN      => SPI_Master,
         LMONO    => Zero,
         TE_EN    => Zero,
         DONE     => In_Progress,
         RXD      => Is_Empty,
         TXD      => Is_Full,
         RXR      => Less_Than_Full,
         RXF      => Not_Full,
         CSPOL0   => Low,
         CSPOL1   => Low,
         CSPOL2   => Low,
         DMA_LEN  => Disable,
         LEN_LONG => Byte,
         RESERVED => Reserved);

      return Result : SPI do
         Result.Node := new SPI_Node'
           (Counter => 1,
            CE1_N   => CE1_N,
            CE0_N   => CE0_N,
            MISO    => MISO,
            MOSI    => MOSI,
            SCLK    => SCLK);
      end return;
   end Create;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out SPI) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out SPI) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter - 1;
         if Self.Node.Counter = 0 then
            Free (Self.Node);
         end if;
      end if;
   end Finalize;

   ---------------------
   -- Set_Master_Mode --
   ---------------------

   procedure Set_Master_Mode
     (Self : SPI;
      Mode : SPI_Master_Mode) is
   begin
      case Mode is
         when Mode_A =>
            SPI0_CS.CPHA_OL := Middle_Low;

         when Mode_B =>
            SPI0_CS.CPHA_OL := Middle_High;

         when Mode_C =>
            SPI0_CS.CPHA_OL := Beginning_Low;

         when Mode_D =>
            SPI0_CS.CPHA_OL := Beginning_High;
      end case;
   end Set_Master_Mode;

   ---------
   -- Get --
   ---------

   function Get (Self : SPI) return Unsigned_Integer_32 is
      Tmp    : SPI0_CS_Register_Type;
      Result : Unsigned_Integer_32;
   begin

      --  10.6.1 Polled (BCM2835 ARM Peripherals)

      SPI0_CS.CLEAR := Both;
      SPI0_CS.TA    := Active;

      loop
         Tmp := SPI0_CS;
         exit when Tmp.RXD = Contains_Data
           or else Tmp.TXD = Has_Space;

         delay (0.000001);
      end loop;

      if Tmp.RXD /= Contains_Data then
         SPI0_FIFO := SPI0_FIFO_Register_Type (16#FF#);

         loop
            Tmp := SPI0_CS;
            exit when Tmp.DONE = Complete;
            delay (0.000001);
         end loop;
      end if;

      Result := Unsigned_Integer_32 (SPI0_FIFO);

      SPI0_CS.TA := Not_Active;

      return Result;
   end Get;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self : SPI;
      Data : Unsigned_Integer_32)
   is
      Tmp : SPI0_CS_Register_Type;
   begin

      --  10.6.1 Polled (BCM2835 ARM Peripherals)
      SPI0_CS.CLEAR := Both;
      SPI0_CS.TA    := Active;

      loop
         Tmp := SPI0_CS;
         exit when Tmp.TXD = Has_Space;
         delay (0.00001);
      end loop;

      SPI0_FIFO := SPI0_FIFO_Register_Type (Data);

      loop
         Tmp := SPI0_CS;
         exit when Tmp.DONE = Complete;
         delay (0.00001);
      end loop;

      SPI0_CS.TA := Not_Active;
   end Send;

   -----------------------
   -- Set_Clock_Divider --
   -----------------------

   Clock_Divider_Values : constant array (SPI_Clock_Divider) of
     SPI0_CLK_CDIV_Clock_Divider :=
       (F_6_kHz   => 0,
        F_12_kHz  => 32768,
        F_24_kHz  => 16384,
        F_48_kHz  => 8192,
        F_97_kHz  => 4096,
        F_195_kHz => 2048,
        F_390_kHz => 1024,
        F_781_kHz => 512,
        F_1_5_MHz => 256,
        F_3_Mhz   => 128,
        F_6_MHz   => 64,
        F_12_MHz  => 32,
        F_25_Mhz  => 16,
        F_50_MHz  => 8,
        F_100_MHz => 4,
        F_200_MHz => 2);

   procedure Set_Clock_Divider
     (Self : SPI;
      Div  : SPI_Clock_Divider) is
   begin
      SPI0_CLK.CDIV := Clock_Divider_Values (Div);
   end Set_Clock_Divider;

end GPIO.SPI;
