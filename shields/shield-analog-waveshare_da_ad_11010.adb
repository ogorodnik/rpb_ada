
with Ada.Unchecked_Conversion;
with Interfaces;               use Interfaces;

with GPIO.Types;               use GPIO.Types;

package body Shield.Analog.WAVESHARE_DA_AD_11010 is

   use GPIO.SPI;

   -- STATUS : STATUS REGISTER (ADDRESS 00h) --

   STATUS_Register_Offset : constant Register_Offset := 16#0#;

   type STATUS_BUFEN_Analog_Input_Buffer_Enable_Type is (Disabled, Enabled);
   for STATUS_BUFEN_Analog_Input_Buffer_Enable_Type use
     (Disabled => 0, Enabled => 1);
   for STATUS_BUFEN_Analog_Input_Buffer_Enable_Type'Size use 1;

   type STATUS_ACAL_Auto_Calibration_Type is (Disabled, Enabled);
   for STATUS_ACAL_Auto_Calibration_Type use
     (Disabled => 0, Enabled => 1);
   for STATUS_ACAL_Auto_Calibration_Type'Size use 1;

   type STATUS_ORDER_Data_Output_Bit_Order_Type is
     (Most_Significant_Bit_First, Least_Significant_Bit_First);
   for STATUS_ORDER_Data_Output_Bit_Order_Type use
     (Most_Significant_Bit_First => 0, Least_Significant_Bit_First => 1);
   for STATUS_ORDER_Data_Output_Bit_Order_Type'Size use 1;

   type STATUS_Factory_Programmed_Identification_Bits_Type
     is range 0 .. 2**4 - 1;
   for STATUS_Factory_Programmed_Identification_Bits_Type'Size use 4;

   type Status_Register_Type is record
      DRDY  : Bit := 0;
      BUFEN : STATUS_BUFEN_Analog_Input_Buffer_Enable_Type;
      ACAL  : STATUS_ACAL_Auto_Calibration_Type;
      ORDER : STATUS_ORDER_Data_Output_Bit_Order_Type;
      ID    : STATUS_Factory_Programmed_Identification_Bits_Type := 0;
   end record;
   for Status_Register_Type use record
      DRDY     at 0 range  0 .. 0;
      BUFEN    at 0 range  1 .. 1;
      ACAL     at 0 range  2 .. 2;
      ORDER    at 0 range  3 .. 3;
      ID       at 0 range  4 .. 7;
   end record;
   for Status_Register_Type'Size use 8;

   function To_Unsigned_Integer_8 is new Ada.Unchecked_Conversion
     (Status_Register_Type, Unsigned_Integer_8);

   -- MUX : Input Multiplexer Control Register (Address 01h) --

   MUX_Register_Offset : constant Register_Offset := 16#1#;

   type MUX_Input_Chanel_Select_Type is
     (AIN_0, AIN_1, AIN_2, AIN_3, AIN_4, AIN_5, AIN_6, AIN_7, AIN_COM);
   for MUX_Input_Chanel_Select_Type use
     (AIN_0   => 2#0000#,
      AIN_1   => 2#0001#,
      AIN_2   => 2#0010#,
      AIN_3   => 2#0011#,
      AIN_4   => 2#0100#,
      AIN_5   => 2#0101#,
      AIN_6   => 2#0110#,
      AIN_7   => 2#0111#,
      AIN_COM => 2#1000#);
   for MUX_Input_Chanel_Select_Type'Size use 4;

   type Input_Multiplexer_Control_Register_Type is record
      NSEL  : MUX_Input_Chanel_Select_Type;
      PSEL  : MUX_Input_Chanel_Select_Type;
   end record;
   for Input_Multiplexer_Control_Register_Type use record
      NSEL at 0 range  0 .. 3;
      PSEL at 0 range  4 .. 7;
   end record;
   for Input_Multiplexer_Control_Register_Type'Size use 8;

   function To_Unsigned_Integer_8 is new Ada.Unchecked_Conversion
       (Input_Multiplexer_Control_Register_Type, Unsigned_Integer_8);

   -- ADCON: A/D Control Register (Address 02h) --

   type ADCON_Programmable_Gain_Amplifier_Setting_Type is
     (PGA_1, PGA_2, PGA_4, PGA_8, PGA_16, PGA_32, PGA_64);
   for ADCON_Programmable_Gain_Amplifier_Setting_Type use
     (PGA_1   => 2#000#,
      PGA_2   => 2#001#,
      PGA_4   => 2#010#,
      PGA_8   => 2#011#,
      PGA_16  => 2#100#,
      PGA_32  => 2#101#,
      PGA_64  => 2#110#);
   for ADCON_Programmable_Gain_Amplifier_Setting_Type'Size use 3;

   type ADCON_Sensor_Detect_Current_Sources_Type is
     (Off, mA_0_5, mA_2, mA_10);
   for ADCON_Sensor_Detect_Current_Sources_Type use
     (Off    => 2#00#,
      mA_0_5 => 2#01#,
      mA_2   => 2#10#,
      mA_10  => 2#11#);
   for ADCON_Sensor_Detect_Current_Sources_Type'Size use 2;

   type ADCON_Clock_Out_Rate_Setting_Type is
     (Off, CLKIN, CLKIN_2, CLKIN_4);
   for ADCON_Clock_Out_Rate_Setting_Type use
     (Off     => 2#00#,
      CLKIN   => 2#01#,
      CLKIN_2 => 2#10#,
      CLKIN_4 => 2#11#);
   for ADCON_Clock_Out_Rate_Setting_Type'Size use 2;

   type AD_Control_Register_Type is record
      PGA      : ADCON_Programmable_Gain_Amplifier_Setting_Type;
      SDSC     : ADCON_Sensor_Detect_Current_Sources_Type;
      CLK      : ADCON_Clock_Out_Rate_Setting_Type;
      RESERVED : Bit := 0;
   end record;
   for AD_Control_Register_Type use record
      PGA      at 0 range  0 .. 2;
      SDSC     at 0 range  3 .. 4;
      CLK      at 0 range  5 .. 6;
      RESERVED at 0 range  7 .. 7;
   end record;
   for AD_Control_Register_Type'Size use 8;

   function To_Unsigned_Integer_8 is new Ada.Unchecked_Conversion
     (AD_Control_Register_Type, Unsigned_Integer_8);

   -- DRATE: A/D Data Rate (Address 03h) --

   type Data_Rate_Setting is
     (SPS_30_000, SPS_15_000, SPS_7_500, SPS_3_750, SPS_2_000, SPS_1_000,
      SPS_500, SPS_100, SPS_60, SPS_50, SPS_30, SPS_25, SPS_15, SPS_10, SPS_5,
      SPS_2_5);

   Data_Rate_Setting_Values : constant
     array (Data_Rate_Setting) of Unsigned_Integer_8 :=
     (SPS_30_000 => 2#11110000#,
      SPS_15_000 => 2#11100000#,
      SPS_7_500  => 2#11010000#,
      SPS_3_750  => 2#11000000#,
      SPS_2_000  => 2#10110000#,
      SPS_1_000  => 2#10100001#,
      SPS_500    => 2#10010010#,
      SPS_100    => 2#10000010#,
      SPS_60     => 2#01110010#,
      SPS_50     => 2#01100011#,
      SPS_30     => 2#01010011#,
      SPS_25     => 2#01000011#,
      SPS_15     => 2#00110011#,
      SPS_10     => 2#00100011#,
      SPS_5      => 2#00010011#,
      SPS_2_5    => 2#00000011#);

   Operations_Delay : constant Duration := 0.00005;

   -- Internals --

   procedure Initialize
     (Board : DA_AD_Board;
      PGA   : ADCON_Programmable_Gain_Amplifier_Setting_Type;
      DRATE : Data_Rate_Setting);
   --  Initializes AD, sets chip settings

   function Command_Write_Register
     (Register : Register_Offset)
      return Unsigned_Integer_8;
   --  Creates command for writing the register

   function Command_Read_Register
     (Register : Register_Offset)
      return Unsigned_Integer_8;
   --  Creates command for reading the register

   function Command_Register
     (Command  : Command_Type;
      Register : Register_Offset)
      return Unsigned_Integer_8;

   function To_Integer_32 is
     new Ada.Unchecked_Conversion (Unsigned_Integer_32, Interfaces.Integer_32);

   ---------------------------
   -- Command_Read_Register --
   ---------------------------

   function Command_Read_Register
     (Register : Register_Offset)
      return Unsigned_Integer_8 is
   begin
      return Command_Register (RREG, Register);
   end Command_Read_Register;

   ----------------------------
   -- Command_Write_Register --
   ----------------------------

   function Command_Write_Register
     (Register : Register_Offset)
      return Unsigned_Integer_8 is
   begin
      return Command_Register (WREG, Register);
   end Command_Write_Register;

   ----------------------
   -- Command_Register --
   ----------------------

   function Command_Register
     (Command  : Command_Type;
      Register : Register_Offset)
      return Unsigned_Integer_8 is
   begin
      return Unsigned_Integer_8
        (Unsigned_8 (Command) or Unsigned_8 (Register));
   end Command_Register;

   ------------
   -- Create --
   ------------

   function Create (Mode : Board_Mode) return DA_AD_Board
   is
      SPI    : GPIO.SPI.SPI;
      SPICS  : GPIO.Output_Pin;
      DRDY   : GPIO.Input_Pin;
      Result : DA_AD_Board;
   begin
      if Mode = DA then
         raise Constraint_Error with "DA mode is unimplemented";
      end if;

      SPI := GPIO.SPI.Create;
      SPI.Set_Master_Mode (Mode_C);
      SPI.Set_Clock_Divider (F_390_kHz);

      SPICS := GPIO.Create (22);
      SPICS.High;

      DRDY := GPIO.Create (17);
      DRDY.Set_Pull_Up_Down (GPIO.Pull_Up);

      Result.Node := new DA_AD_Board_Node'
        (Counter => 1,
         SPI     => SPI,
         SPICS   => SPICS,
         DRDY    => DRDY);

      Initialize (Result, PGA_1, SPS_15);

      return Result;
   end Create;

   ------------------
   -- Get_Register --
   ------------------

   function Get_Register
     (Self     : DA_AD_Board;
      Register : Register_Offset)
      return Interfaces.Unsigned_8
   is
      Result : Interfaces.Unsigned_8;
   begin
      DA_AD_Board_Node_Access (Self.Node).SPICS.Low;
      Self.ISP_Send (Command_Read_Register (Register));
      Self.ISP_Send (0);
      delay (0.00001);

      Result := Interfaces.Unsigned_8
        (DA_AD_Board_Node_Access (Self.Node).SPI.Get);
      DA_AD_Board_Node_Access (Self.Node).SPICS.High;

      return Result;
   end Get_Register;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Board : DA_AD_Board;
      PGA   : ADCON_Programmable_Gain_Amplifier_Setting_Type;
      DRATE : Data_Rate_Setting) is
   begin

      --  WREG: Write to Register (ADS1256.pdf p.36)

      if not DA_AD_Board_Node_Access (Board.Node).DRDY.Wait_Low (0.5) then
         raise Program_Error with "Can't configure ADC, DDRY is not ready";
      end if;

      DA_AD_Board_Node_Access (Board.Node).SPICS.Low;

      --  1st Command Byte: 0101 rrrr where rrrr is the address to the first
      --  register to be written.
      Board.ISP_Send (Command_Write_Register (STATUS_Register_Offset));

      --  2nd Command Byte: 0000 nnnn where nnnn is the number of bytes
      --  to be writtent -1
      Board.ISP_Send (3);

      --  Data Byte(s): data to be written to the registers
      Board.ISP_Send
        (To_Unsigned_Integer_8
           (Status_Register_Type'
                (DRDY  => 0,
                 BUFEN => Disabled,
                 ACAL  => Enabled,
                 ORDER => Most_Significant_Bit_First,
                 ID    => 0)));

      Board.ISP_Send
        (To_Unsigned_Integer_8
           (Input_Multiplexer_Control_Register_Type'
                (NSEL => AIN_COM, PSEL => AIN_0)));

      Board.ISP_Send
        (To_Unsigned_Integer_8
           (AD_Control_Register_Type'
                (PGA => PGA, SDSC => Off, CLK => Off, RESERVED => 0)));

      Board.ISP_Send (Data_Rate_Setting_Values (DRATE));

      DA_AD_Board_Node_Access (Board.Node).SPICS.High;
      delay (Operations_Delay);
   end Initialize;

   -------------
   -- Chip_Id --
   -------------

   function Chip_Id (Self : DA_AD_Board) return Integer is
   begin
      if not DA_AD_Board_Node_Access (Self.Node).DRDY.Wait_Low (0.5) then
         raise Program_Error with "Can't get Chip_Id, DDRY is not ready";
      end if;

      return Integer
        (Shift_Right (Self.Get_Register (STATUS_Register_Offset), 4));
   end Chip_Id;

   --------------
   -- ISP_Send --
   --------------

   procedure ISP_Send
     (Self : DA_AD_Board;
      Data : Unsigned_Integer_8) is
   begin
      delay (0.000002);
      DA_AD_Board_Node_Access (Self.Node).SPI.Send
        (Unsigned_Integer_32 (Data));
   end ISP_Send;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (Self : DA_AD_Board) return Long_Float is
      Node : constant DA_AD_Board_Node_Access :=
        DA_AD_Board_Node_Access (Self.Node);

      Data : Unsigned_32 := 0;
   begin
      Node.SPICS.Low;

      Self.ISP_Send (Unsigned_Integer_8 (RDATA));
      delay (0.00001);

      Data := Shift_Left (Unsigned_32 (Node.SPI.Get), 16) and 16#00FF0000#;
      Data := Data or Shift_Left (Unsigned_32 (Node.SPI.Get), 8);
      Data := Data or Unsigned_32 (Node.SPI.Get);

      Node.SPICS.High;

      if (Data and 16#800000#) /= 0 then
         return Long_Float (Data or 16#FF000000#) * 100.0 / 167.0;
      else
         return Long_Float (Data) * 100.0 / 167.0;
      end if;
   end Read_Data;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command
     (Self    : DA_AD_Board;
      Command : Command_Type) is
   begin
      DA_AD_Board_Node_Access (Self.Node).SPICS.Low;
      Self.ISP_Send (Unsigned_Integer_8 (Command));
      DA_AD_Board_Node_Access (Self.Node).SPICS.High;
   end Send_Command;

   ------------------
   -- Set_Register --
   ------------------

   procedure Set_Register
     (Self     : DA_AD_Board;
      Register : Register_Offset;
      Data     : Unsigned_Integer_8) is
   begin
      DA_AD_Board_Node_Access (Self.Node).SPICS.Low;
      Self.ISP_Send (Command_Write_Register (Register));
      Self.ISP_Send (0);
      Self.ISP_Send (Data);
      DA_AD_Board_Node_Access (Self.Node).SPICS.High;
   end Set_Register;

   -----------
   -- Value --
   -----------

   Chanel : constant array (AD_Input_Number) of MUX_Input_Chanel_Select_Type :=
     (0 => AIN_0, 1 => AIN_1, 2 => AIN_2, 3 => AIN_3,
      4 => AIN_4, 5 => AIN_5, 6 => AIN_6, 7 => AIN_7);

   overriding function Value
     (Self : DA_AD_Board;
      Pin  : Input_Pin_Number)
      return Long_Float is
   begin
      loop
         exit when DA_AD_Board_Node_Access (Self.Node).DRDY.Wait_Low (0.5);
      end loop;

      --  Select chanel PSEL = AIN0
      Self.Set_Register
        (MUX_Register_Offset,
         To_Unsigned_Integer_8
           (Input_Multiplexer_Control_Register_Type'
                (NSEL => AIN_COM,
                 PSEL => Chanel (AD_Input_Number (Pin)))));
      delay (0.000005);

      Self.Send_Command (SYNC);
      delay (0.000005);

      Self.Send_Command (WAKEUP);
      delay (0.000025);

      return Self.Read_Data;
   end Value;

end Shield.Analog.WAVESHARE_DA_AD_11010;
