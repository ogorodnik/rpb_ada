--------------------------------------
--  Raspberry Pi3 ( bcm2837B0 ) I2C --
--------------------------------------

with Ada.Unchecked_Deallocation;
with Peripherals;

package body GPIO.I2C is

   BSC0_Base_Address : constant Address :=
     Peripherals.Base_Address + 16#20_5000#;
   BSC1_Base_Address : constant Address :=
     Peripherals.Base_Address + 16#80_4000#;

   -- Control register --

   BSC0_C_Address : constant Address := BSC0_Base_Address;
   BSC1_C_Address : constant Address := BSC1_Base_Address;

   type Control_Read_Transfer is (Write_Packet_Transfer, Read_Packet_Transfer);
   for Control_Read_Transfer use
     (Write_Packet_Transfer => 0,
      Read_Packet_Transfer  => 1);
   for Control_Read_Transfer'Size use 1;

   type Control_1_3_Reserved is (Zero);
   for Control_1_3_Reserved use (Zero => 0);
   for Control_1_3_Reserved'Size use 3;

   type Control_Clear_FIFO is (No_Action, Clear, Clear1);
   for Control_Clear_FIFO use
     (No_Action => 0,
      Clear     => 1,
      Clear1    => 3);
   for Control_Clear_FIFO'Size use 2;

   type Control_6_Reserved is (Zero);
   for Control_6_Reserved use (Zero => 0);
   for Control_6_Reserved'Size use 1;

   type Control_Start_Transfer is (No_Action, Start_Transfer);
   for Control_Start_Transfer use
     (No_Action      => 0,
      Start_Transfer => 1);
   for Control_Start_Transfer'Size use 1;

   type Control_Interrupt_On_Done is (Dont_Generate, Generate);
   for Control_Interrupt_On_Done use
     (Dont_Generate => 0,
      Generate      => 1);
   for Control_Interrupt_On_Done'Size use 1;

   type Control_Interrupt_On_TX is (Dont_Generate, Generate);
   for Control_Interrupt_On_TX use
     (Dont_Generate => 0,
      Generate      => 1);
   for Control_Interrupt_On_TX'Size use 1;

   type Control_Interrupt_On_RX is (Dont_Generate, Generate);
   for Control_Interrupt_On_RX use
     (Dont_Generate => 0,
      Generate      => 1);
   for Control_Interrupt_On_RX'Size use 1;

   type Control_11_14_Reserved is (Zero);
   for Control_11_14_Reserved use (Zero => 0);
   for Control_11_14_Reserved'Size use 4;

   type Control_I2C_Enable is (Disabled, Enabled);
   for Control_I2C_Enable use
     (Disabled => 0,
      Enabled  => 1);
   for Control_I2C_Enable'Size use 1;

   type Control_16_31_Reserved is (Zero);
   for Control_16_31_Reserved use (Zero => 0);
   for Control_16_31_Reserved'Size use 16;

   type Control_Register_Type is record
      READ      : Control_Read_Transfer;
      RESERVED1 : Control_1_3_Reserved;
      CLEAR     : Control_Clear_FIFO;
      RESERVED2 : Control_6_Reserved;
      ST        : Control_Start_Transfer;
      INTD      : Control_Interrupt_On_Done;
      INTT      : Control_Interrupt_On_TX;
      INTR      : Control_Interrupt_On_RX;
      RESERVED3 : Control_11_14_Reserved;
      I2CEN     : Control_I2C_Enable;
      RESERVED4 : Control_16_31_Reserved;
   end record;
   for Control_Register_Type use record
      READ      at 0 range  0 .. 0;
      RESERVED1 at 0 range  1 .. 3;
      CLEAR     at 0 range  4 .. 5;
      RESERVED2 at 0 range  6 .. 6;
      ST        at 0 range  7 .. 7;
      INTD      at 0 range  8 .. 8;
      INTT      at 0 range  9 .. 9;
      INTR      at 0 range 10 .. 10;
      RESERVED3 at 0 range 11 .. 14;
      I2CEN     at 0 range 15 .. 15;
      RESERVED4 at 0 range 16 .. 31;
   end record;
   for Control_Register_Type'Size use 32;

   BSC0_C : Control_Register_Type;
   for BSC0_C'Address use BSC0_C_Address;
   pragma Volatile (BSC0_C);
   pragma Atomic (BSC0_C);

   BSC1_C : Control_Register_Type;
   for BSC1_C'Address use BSC1_C_Address;
   pragma Volatile (BSC1_C);
   pragma Atomic (BSC1_C);

   -- Status register --

   BSC0_S_Address : constant Address := BSC0_Base_Address + 16#04#;
   BSC1_S_Address : constant Address := BSC1_Base_Address + 16#04#;

   type Status_Transfer_Active is (Not_Active, Active);
   for Status_Transfer_Active use
     (Not_Active => 0,
      Active     => 1);
   for Status_Transfer_Active'Size use 1;

   type Status_Transfer_Done is (Not_Completed, Completed);
   for Status_Transfer_Done use
     (Not_Completed => 0,
      Completed     => 1);
   for Status_Transfer_Done'Size use 1;

   type Status_FIFO_Needs_Writing is (Full, Not_Full);
   for Status_FIFO_Needs_Writing use
     (Full     => 0,
      Not_Full => 1);
   for Status_FIFO_Needs_Writing'Size use 1;

   type Status_FIFO_Needs_Reading is (Not_Full, Full);
   for Status_FIFO_Needs_Reading use
     (Not_Full => 0,
      Full     => 1);
   for Status_FIFO_Needs_Reading'Size use 1;

   type Status_FIFO_Can_Accept_Data is (Full, Not_Full);
   for Status_FIFO_Can_Accept_Data use
     (Full     => 0,
      Not_Full => 1);
   for Status_FIFO_Can_Accept_Data'Size use 1;

   type Status_FIFO_Contains_Data is (Empty, Not_Empty);
   for Status_FIFO_Contains_Data use
     (Empty     => 0,
      Not_Empty => 1);
   for Status_FIFO_Contains_Data'Size use 1;

   type Status_FIFO_Empty is (Not_Empty, Empty);
   for Status_FIFO_Empty use
     (Not_Empty => 0,
      Empty     => 1);
   for Status_FIFO_Empty'Size use 1;

   type Status_FIFO_Full is (Not_Full, Full);
   for Status_FIFO_Full use
     (Not_Full => 0,
      Full     => 1);
   for Status_FIFO_Full'Size use 1;

   type Status_ACK_Error is (No_Errors, Slave_Not_Addressed);
   for Status_ACK_Error use
     (No_Errors           => 0,
      Slave_Not_Addressed => 1);
   for Status_ACK_Error'Size use 1;

   type Status_Clock_Stretch_Timeout is (No_Errors, Slave_SCL_Signal);
   for Status_Clock_Stretch_Timeout use
     (No_Errors        => 0,
      Slave_SCL_Signal => 1);
   for Status_Clock_Stretch_Timeout'Size use 1;

   type Status_Reserved is (Zero);
   for Status_Reserved use (Zero => 0);
   for Status_Reserved'Size use 22;

   type Status_Register_Type is record
      TA       : Status_Transfer_Active;
      DONE     : Status_Transfer_Done;
      TXW      : Status_FIFO_Needs_Writing;
      RXR      : Status_FIFO_Needs_Reading;
      TXD      : Status_FIFO_Can_Accept_Data;
      RXD      : Status_FIFO_Contains_Data;
      TXE      : Status_FIFO_Empty;
      RXF      : Status_FIFO_Full;
      ERR      : Status_ACK_Error;
      CLKT     : Status_Clock_Stretch_Timeout;
      RESERVED : Status_Reserved;
   end record;
   for Status_Register_Type use record
      TA       at 0 range  0 .. 0;
      DONE     at 0 range  1 .. 1;
      TXW      at 0 range  2 .. 2;
      RXR      at 0 range  3 .. 3;
      TXD      at 0 range  4 .. 4;
      RXD      at 0 range  5 .. 5;
      TXE      at 0 range  6 .. 6;
      RXF      at 0 range  7 .. 7;
      ERR      at 0 range  8 .. 8;
      CLKT     at 0 range  9 .. 9;
      RESERVED at 0 range 10 .. 31;
   end record;
   for Status_Register_Type'Size use 32;

   BSC0_S : Status_Register_Type;
   for BSC0_S'Address use BSC0_S_Address;
   pragma Volatile (BSC0_S);
   pragma Atomic (BSC0_S);

   BSC1_S : Status_Register_Type;
   for BSC1_S'Address use BSC1_S_Address;
   pragma Volatile (BSC1_S);
   pragma Atomic (BSC1_S);

   -- Data Length register --

   BSC0_DLEN_Address : constant Address := BSC0_Base_Address + 16#08#;
   BSC1_DLEN_Address : constant Address := BSC1_Base_Address + 16#08#;

   type Data_Length_Length is range 0 .. 2**16 - 1;
   for Data_Length_Length'Size use 16;

   type Data_Length_Reserved is (Zero);
   for Data_Length_Reserved use (Zero => 0);
   for Data_Length_Reserved'Size use 16;

   type Data_Length_Register_Type is record
      DLEN     : Data_Length_Length;
      RESERVED : Data_Length_Reserved;
   end record;
   for Data_Length_Register_Type use record
      DLEN     at 0 range  0 .. 15;
      RESERVED at 0 range 16 .. 31;
   end record;
   for Data_Length_Register_Type'Size use 32;

   BSC0_DLEN : Data_Length_Register_Type;
   for BSC0_DLEN'Address use BSC0_DLEN_Address;
   pragma Volatile (BSC0_DLEN);
   pragma Atomic (BSC0_DLEN);

   BSC1_DLEN : Data_Length_Register_Type;
   for BSC1_DLEN'Address use BSC1_DLEN_Address;
   pragma Volatile (BSC1_DLEN);
   pragma Atomic (BSC1_DLEN);

   -- Slave Address register --

   BSC0_A_Address : constant Address := BSC0_Base_Address + 16#0C#;
   BSC1_A_Address : constant Address := BSC1_Base_Address + 16#0C#;

   type Slave_Address_Reserved is (Zero);
   for Slave_Address_Reserved use (Zero => 0);
   for Slave_Address_Reserved'Size use 25;

   type Slave_Address_Register_Type is record
      ADDR     : Slave_Address_7Bit_WR;
      RESERVED : Slave_Address_Reserved;
   end record;
   for Slave_Address_Register_Type use record
      ADDR     at 0 range 0 .. 6;
      RESERVED at 0 range 7 .. 31;
   end record;
   for Slave_Address_Register_Type'Size use 32;

   BSC0_A : Slave_Address_Register_Type;
   for BSC0_A'Address use BSC0_A_Address;
   pragma Volatile (BSC0_A);
   pragma Atomic (BSC0_A);

   BSC1_A : Slave_Address_Register_Type;
   for BSC1_A'Address use BSC1_A_Address;
   pragma Volatile (BSC1_A);
   pragma Atomic (BSC1_A);

   -- Data FIFO register --

   BSC0_FIFO_Address : constant Address := BSC0_Base_Address + 16#10#;
   BSC1_FIFO_Address : constant Address := BSC1_Base_Address + 16#10#;

   type FIFO_Reserved is (Zero);
   for FIFO_Reserved use (Zero => 0);
   for FIFO_Reserved'Size use 24;

   type FIFO_Register_Type is record
      DATA     : Unsigned_Integer_8;
      RESERVED : FIFO_Reserved;
   end record;
   for FIFO_Register_Type use record
      DATA     at 0 range 0 .. 7;
      RESERVED at 0 range 8 .. 31;
   end record;
   for FIFO_Register_Type'Size use 32;

   BSC0_FIFO : FIFO_Register_Type;
   for BSC0_FIFO'Address use BSC0_FIFO_Address;
   pragma Volatile (BSC0_FIFO);
   pragma Atomic (BSC0_FIFO);

   BSC1_FIFO : FIFO_Register_Type;
   for BSC1_FIFO'Address use BSC1_FIFO_Address;
   pragma Volatile (BSC1_FIFO);
   pragma Atomic (BSC1_FIFO);

   -- Clock Divider register --

   BSC0_DIV_Address : constant Address := BSC0_Base_Address + 16#14#;
   BSC1_DIV_Address : constant Address := BSC1_Base_Address + 16#14#;

   type Clock_Divider_Reserved is (Zero);
   for Clock_Divider_Reserved use (Zero => 0);
   for Clock_Divider_Reserved'Size use 16;

   type Clock_Divider_Register_Type is record
      CDIV     : I2C_Clock_Divider;
      RESERVED : Clock_Divider_Reserved;
   end record;
   for Clock_Divider_Register_Type use record
      CDIV     at 0 range  0 .. 15;
      RESERVED at 0 range 16 .. 31;
   end record;
   for Clock_Divider_Register_Type'Size use 32;

   BSC0_DIV : Clock_Divider_Register_Type;
   for BSC0_DIV'Address use BSC0_DIV_Address;
   pragma Volatile (BSC0_DIV);
   pragma Atomic (BSC0_DIV);

   BSC1_DIV : Clock_Divider_Register_Type;
   for BSC1_DIV'Address use BSC1_DIV_Address;
   pragma Volatile (BSC1_DIV);
   pragma Atomic (BSC1_DIV);

   -- Data Delay register --

   BSC0_DEL_Address : constant Address := BSC0_Base_Address + 16#18#;
   BSC1_DEL_Address : constant Address := BSC1_Base_Address + 16#18#;

   type Data_Delay_Rising is range 0 .. 2**16 - 1;
   for Data_Delay_Rising'Size use 16;

   type Data_Delay_Falling is range 0 .. 2**16 - 1;
   for Data_Delay_Falling'Size use 16;

   type Data_Delay_Register_Type is record
      REDL : Data_Delay_Rising;
      FEDL : Data_Delay_Falling;
   end record;
   for Data_Delay_Register_Type use record
      REDL at 0 range  0 .. 15;
      FEDL at 0 range 16 .. 31;
   end record;
   for Data_Delay_Register_Type'Size use 32;

   BSC0_DEL : Data_Delay_Register_Type;
   for BSC0_DEL'Address use BSC0_DEL_Address;
   pragma Volatile (BSC0_DEL);
   pragma Atomic (BSC0_DEL);

   BSC1_DEL : Data_Delay_Register_Type;
   for BSC1_DEL'Address use BSC1_DEL_Address;
   pragma Volatile (BSC1_DEL);
   pragma Atomic (BSC1_DEL);

   -- Clock Stretch Timeout register --

   BSC0_CLKT_Address : constant Address := BSC0_Base_Address + 16#1C#;
   BSC1_CLKT_Address : constant Address := BSC1_Base_Address + 16#1C#;

   type Clock_Stretch_Timeout_Value is range 0 .. 2**16 - 1;
   for Clock_Stretch_Timeout_Value'Size use 16;

   type Clock_Stretch_Timeout_Reserved is (Zero);
   for Clock_Stretch_Timeout_Reserved use (Zero => 0);
   for Clock_Stretch_Timeout_Reserved'Size use 16;

   type Clock_Stretch_Timeout_Register_Type is record
      TOUT     : Clock_Stretch_Timeout_Value;
      RESERVED : Clock_Stretch_Timeout_Reserved;
   end record;
   for Clock_Stretch_Timeout_Register_Type use record
      TOUT     at 0 range  0 .. 15;
      RESERVED at 0 range 16 .. 31;
   end record;
   for Clock_Stretch_Timeout_Register_Type'Size use 32;

   BSC0_CLKT : Clock_Stretch_Timeout_Register_Type;
   for BSC0_CLKT'Address use BSC0_CLKT_Address;
   pragma Volatile (BSC0_CLKT);
   pragma Atomic (BSC0_CLKT);

   BSC1_CLKT : Clock_Stretch_Timeout_Register_Type;
   for BSC1_CLKT'Address use BSC1_CLKT_Address;
   pragma Volatile (BSC1_CLKT);
   pragma Atomic (BSC1_CLKT);

   -- Internal --

   procedure Free is
     new Ada.Unchecked_Deallocation (I2C_Node, I2C_Node_Access);

   Counter   : Natural := 0;
   I2C_Data  : Alternate_Pin;
   I2C_Clock : Alternate_Pin;
   Created   : array (BSC_Number) of Boolean := (others => False);

   procedure Set_GPIO;
   procedure Release_GPIO;

   procedure Clear_FIFO (Self : I2C);

   procedure Clear_Status (Self : I2C);
   procedure Set_Done (Self : I2C);

   function  Get_Control_Register (Self : I2C) return Control_Register_Type;
   procedure Set_Control_Register (Self : I2C; R : Control_Register_Type);

   function  Get_Status_Register (Self : I2C) return Status_Register_Type;
   procedure Set_Status_Register (Self : I2C; R : Status_Register_Type);

   procedure Set_Data_Length_Register
     (Self : I2C; R : Data_Length_Register_Type);

   --------------
   -- Set_GPIO --
   --------------

   procedure Set_GPIO is
   begin
      if Counter = 0 then
         I2C_Data  := Create (2,  Alt_0);
         I2C_Clock := Create (3,  Alt_0);
      end if;
      Counter := Counter + 1;
   end Set_GPIO;

   ------------------
   -- Release_GPIO --
   ------------------

   procedure Release_GPIO is
   begin
      Counter := Counter - 1;
      if Counter = 0 then
         I2C_Data  := Empty_Alternate_Pin;
         I2C_Clock := Empty_Alternate_Pin;
      end if;
   end Release_GPIO;

   ------------
   -- Create --
   ------------

   function Create return I2C_BSC0 is
   begin
      if Created (BSC0) then
         raise Program_Error with "I2C(BSC0) is already created";
      end if;

      Set_GPIO;

      return Result : I2C_BSC0 do
         Created (BSC0) := True;
         Result.Node := new I2C_Node'
           (Counter => 1,
            Number  => BSC0,
            Address => (Addr => 0, WR => Read),
            Timeout => 0.0);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return I2C_BSC1 is
   begin
      if Created (BSC1) then
         raise Program_Error with "I2C(BSC1) is already created";
      end if;

      Set_GPIO;

      return Result : I2C_BSC1 do
         Created (BSC1) := True;
         Result.Node := new I2C_Node'
           (Counter => 1,
            Number  => BSC1,
            Address => (Addr => 0, WR => Read),
            Timeout => 0.0);
      end return;
   end Create;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out I2C) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out I2C) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter - 1;
         if Self.Node.Counter = 0 then
            Release_GPIO;
            Created (Self.Node.Number) := False;
            Free (Self.Node);
         end if;
      end if;
   end Finalize;

   -----------------------
   -- Set_Slave_Address --
   -----------------------

   procedure Set_Slave_Address
     (Self    : I2C;
      Address : Slave_Address_7Bit_WR;
      Divider : I2C_Clock_Divider)
   is
      A : constant Slave_Address_Register_Type :=
        (ADDR => Address, RESERVED => Zero);
      D : constant Clock_Divider_Register_Type :=
        (CDIV => Divider, RESERVED => Zero);

   begin
      if Self.Node.Address /= Address then
         case Self.Node.Number is
            when BSC0 =>
               BSC0_A   := A;
               BSC0_DIV := D;

            when BSC1 =>
               BSC1_A   := A;
               BSC1_DIV := D;
         end case;

         Self.Node.Address := Address;
         Self.Node.Timeout := Integer (Divider) / 250000000 * 9000000.0;
      end if;
   end Set_Slave_Address;

   --------------------------
   -- Get_Control_Register --
   --------------------------

   function Get_Control_Register (Self : I2C) return Control_Register_Type is
   begin
      case Self.Node.Number is
         when BSC0 =>
            return BSC0_C;

         when BSC1 =>
            return BSC1_C;
      end case;
   end Get_Control_Register;

   --------------------------
   -- Set_Control_Register --
   --------------------------

   procedure Set_Control_Register (Self : I2C; R : Control_Register_Type) is
   begin
      case Self.Node.Number is
         when BSC0 =>
            BSC0_C := R;

         when BSC1 =>
            BSC1_C := R;
      end case;
   end Set_Control_Register;

   -------------------------
   -- Get_Status_Register --
   -------------------------

   function Get_Status_Register (Self : I2C) return Status_Register_Type is
   begin
      case Self.Node.Number is
         when BSC0 =>
            return BSC0_S;

         when BSC1 =>
            return BSC1_S;
      end case;
   end Get_Status_Register;

   -------------------------
   -- Set_Status_Register --
   -------------------------

   procedure Set_Status_Register (Self : I2C; R : Status_Register_Type) is
   begin
      case Self.Node.Number is
         when BSC0 =>
            BSC0_S := R;

         when BSC1 =>
            BSC1_S := R;
      end case;
   end Set_Status_Register;

   ------------------------------
   -- Set_Data_Length_Register --
   ------------------------------

   procedure Set_Data_Length_Register
     (Self : I2C; R : Data_Length_Register_Type) is
   begin
      case Self.Node.Number is
         when BSC0 =>
            BSC0_DLEN := R;

         when BSC1 =>
            BSC1_DLEN := R;
      end case;
   end Set_Data_Length_Register;

   ----------------
   -- Clear_FIFO --
   ----------------

   procedure Clear_FIFO (Self : I2C)
   is
      C : Control_Register_Type;
   begin
      C := Get_Control_Register (Self);
      C.CLEAR := Clear1;
      Set_Control_Register (Self, C);
   end Clear_FIFO;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (Self : I2C)
   is
      S : Status_Register_Type;
   begin
      S :=
        (TA       => Status_Transfer_Active'First,
         DONE     => Completed,
         TXW      => Status_FIFO_Needs_Writing'First,
         RXR      => Status_FIFO_Needs_Reading'First,
         TXD      => Status_FIFO_Can_Accept_Data'First,
         RXD      => Status_FIFO_Contains_Data'First,
         TXE      => Status_FIFO_Empty'First,
         RXF      => Status_FIFO_Full'First,
         ERR      => Slave_Not_Addressed,
         CLKT     => Slave_SCL_Signal,
         RESERVED => Status_Reserved'First);

      Set_Status_Register (Self, S);
   end Clear_Status;

   --------------
   -- Set_Done --
   --------------

   procedure Set_Done (Self : I2C)
   is
      S : Status_Register_Type;
   begin
      S := Get_Status_Register (Self);
      S.DONE := Completed;
      Set_Status_Register (Self, S);
   end Set_Done;

   ----------
   -- Send --
   ----------

   function Send
     (Self    : I2C;
      Address : Slave_Address_7Bit;
      Divider : I2C_Clock_Divider;
      Data    : Unsigned_Integer_8_Array)
      return Send_Result
   is
      C   : Control_Register_Type;
      S   : Status_Register_Type;
      Idx : Positive;

      procedure Write (Data : Unsigned_Integer_8);
      procedure Write (Data : Unsigned_Integer_8) is
      begin
         case Self.Node.Number is
            when BSC0 =>
               BSC0_FIFO := (DATA => Data, RESERVED => Zero);

            when BSC1 =>
               BSC1_FIFO := (DATA => Data, RESERVED => Zero);
         end case;
      end Write;

   begin
      -- Set address
      Self.Set_Slave_Address ((ADDR => Address, WR => Write), Divider);
      Clear_FIFO (Self);
      Clear_Status (Self);

      Set_Data_Length_Register (Self, (DLEN => Data'Length, RESERVED => Zero));

      --  push data into the internal buffer which has 16 bytes
      Idx := Positive'Min (Data'Last - Data'First + 1, 16);

      for Index in Data'First .. Idx loop
         Write (Data (Index));
      end loop;
      Idx := Data'First + 16;

      --  Start transfer
      C :=
        (READ      => Write_Packet_Transfer,
         RESERVED1 => Control_1_3_Reserved'First,
         CLEAR     => Control_Clear_FIFO'First,
         RESERVED2 => Control_6_Reserved'First,
         ST        => Start_Transfer,
         INTD      => Control_Interrupt_On_Done'First,
         INTT      => Control_Interrupt_On_TX'First,
         INTR      => Control_Interrupt_On_RX'First,
         RESERVED3 => Control_11_14_Reserved'First,
         I2CEN     => Enabled,
         RESERVED4 => Control_16_31_Reserved'First);

      Set_Control_Register (Self, C);

      loop
         S := Get_Status_Register (Self);
         exit when S.DONE = Completed;

         if Idx <= Data'Last
           and then S.TXD = Not_Full
         then
            --  We have not written data and have space in the buffer,
            --  write one byte.

            Write (Data (Idx));
            Idx := Idx + 1;
         end if;
      end loop;

      S := Get_Status_Register (Self);

      if S.ERR = Slave_Not_Addressed then
         Set_Done (Self);
         return Send_Result'(Kind => Wrong_Address);

      elsif S.CLKT = Slave_SCL_Signal then
         Set_Done (Self);
         return Send_Result'(Kind => Clock_Stretching);

      elsif Idx <= Data'Last then
         Set_Done (Self);
         return Send_Result'
           (Kind => Data_Left, Last_Written_Index => Idx - 1);

      else
         Set_Done (Self);
         return Send_Result'(Kind => No_Error);
      end if;
   end Send;

   ---------
   -- Get --
   ---------

   function Get
     (Self    : I2C;
      Address : Slave_Address_7Bit;
      Divider : I2C_Clock_Divider)
      return Unsigned_Integer_8
   is
      Result : Unsigned_Integer_8;
      S      : Status_Register_Type;
      C      : Control_Register_Type;

      function Read return Unsigned_Integer_8;
      function Read return Unsigned_Integer_8
      is
         F : FIFO_Register_Type;
      begin
         case Self.Node.Number is
            when BSC0 =>
               F := BSC0_FIFO;

            when BSC1 =>
               F := BSC1_FIFO;
         end case;

         return F.DATA;
      end Read;

   begin
      -- Set address
      Self.Set_Slave_Address ((ADDR => Address, WR => Read), Divider);
      Clear_FIFO (Self);
      Clear_Status (Self);

      Set_Data_Length_Register (Self, (DLEN => 1, RESERVED => Zero));

      --  Start read
      C :=
        (READ      => Read_Packet_Transfer,
         RESERVED1 => Control_1_3_Reserved'First,
         CLEAR     => Control_Clear_FIFO'First,
         RESERVED2 => Control_6_Reserved'First,
         ST        => Start_Transfer,
         INTD      => Control_Interrupt_On_Done'First,
         INTT      => Control_Interrupt_On_TX'First,
         INTR      => Control_Interrupt_On_RX'First,
         RESERVED3 => Control_11_14_Reserved'First,
         I2CEN     => Enabled,
         RESERVED4 => Control_16_31_Reserved'First);

      Set_Control_Register (Self, C);

      loop
         S := Get_Status_Register (Self);
         exit when S.DONE = Completed;

         if S.RXD = Not_Empty then
            Result := Read;
         end if;
      end loop;

      loop
         S := Get_Status_Register (Self);
         exit when S.RXD = Empty;

         Result := Read;
      end loop;

      Set_Done (Self);

      return Result;
   end Get;

end GPIO.I2C;
