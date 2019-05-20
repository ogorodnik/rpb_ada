
with Ada.Calendar;                use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Interfaces.C;                use Interfaces.C;

with Peripherals;

package body GPIO is

   GPIO_Base_Address : constant Address :=
     Peripherals.Base_Address + 16#20_0000#;

   -- GPFSEL register repreesentation type --

   type GPFSEL_Reserved is (Reserved);
   for GPFSEL_Reserved use (Reserved => 0);
   for GPFSEL_Reserved'Size use 2;

   type GPIO_Select_Register_Type is record
      FSEL0    : GPIO_FSEL_Mode;
      FSEL1    : GPIO_FSEL_Mode;
      FSEL2    : GPIO_FSEL_Mode;
      FSEL3    : GPIO_FSEL_Mode;
      FSEL4    : GPIO_FSEL_Mode;
      FSEL5    : GPIO_FSEL_Mode;
      FSEL6    : GPIO_FSEL_Mode;
      FSEL7    : GPIO_FSEL_Mode;
      FSEL8    : GPIO_FSEL_Mode;
      FSEL9    : GPIO_FSEL_Mode;
      RESERVED : GPFSEL_Reserved;
   end record;
   for GPIO_Select_Register_Type use record
      FSEL0    at 0 range  0 .. 2;
      FSEL1    at 0 range  3 .. 5;
      FSEL2    at 0 range  6 .. 8;
      FSEL3    at 0 range  9 .. 11;
      FSEL4    at 0 range 12 .. 14;
      FSEL5    at 0 range 15 .. 17;
      FSEL6    at 0 range 18 .. 20;
      FSEL7    at 0 range 21 .. 23;
      FSEL8    at 0 range 24 .. 26;
      FSEL9    at 0 range 27 .. 29;
      RESERVED at 0 range 30 .. 31;
   end record;
   for GPIO_Select_Register_Type'Size use 32;

   -- GPFSELn addresses --

   GPIO_Select_0_Address : constant Address := GPIO_Base_Address;
   GPIO_Select_1_Address : constant Address := GPIO_Base_Address + 16#4#;
   GPIO_Select_2_Address : constant Address := GPIO_Base_Address + 16#8#;

   -- GPFSELn --

   GPIO_Select_0 : aliased GPIO_Select_Register_Type;
   for GPIO_Select_0'Address use GPIO_Select_0_Address;
   pragma Volatile (GPIO_Select_0);
   pragma Atomic (GPIO_Select_0);

   GPIO_Select_1 : aliased GPIO_Select_Register_Type;
   for GPIO_Select_1'Address use GPIO_Select_1_Address;
   pragma Volatile (GPIO_Select_1);
   pragma Atomic (GPIO_Select_1);

   GPIO_Select_2 : aliased GPIO_Select_Register_Type;
   for GPIO_Select_2'Address use GPIO_Select_2_Address;
   pragma Volatile (GPIO_Select_2);
   pragma Atomic (GPIO_Select_2);

   -- GPSET and GPCLR register repreesentation type --

   type GPIO_Set_Clear_Register_Type is range 0 .. 2**32 - 1;
   for GPIO_Set_Clear_Register_Type'Size use 32;

   -- GPSETn addresses --
   GPIO_Output_Set_0_Address : constant Address := GPIO_Base_Address + 16#1C#;
   GPIO_Output_Set_1_Address : constant Address := GPIO_Base_Address + 16#20#;

   -- GPSETn --
   GPIO_Output_Set_0 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Set_0'Address use GPIO_Output_Set_0_Address;
   pragma Volatile (GPIO_Output_Set_0);
   pragma Atomic (GPIO_Output_Set_0);

   GPIO_Output_Set_1 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Set_1'Address use GPIO_Output_Set_1_Address;
   pragma Volatile (GPIO_Output_Set_1);
   pragma Atomic (GPIO_Output_Set_1);

   -- GPCLRn addresses --
   GPIO_Output_Clear_0_Address : constant Address :=
     GPIO_Base_Address + 16#28#;
   GPIO_Output_Clear_1_Address : constant Address :=
     GPIO_Base_Address + 16#2C#;

   -- GPCLRn --
   GPIO_Output_Clear_0 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Clear_0'Address use GPIO_Output_Clear_0_Address;
   pragma Volatile (GPIO_Output_Clear_0);
   pragma Atomic (GPIO_Output_Clear_0);

   GPIO_Output_Clear_1 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Clear_1'Address use GPIO_Output_Clear_1_Address;
   pragma Volatile (GPIO_Output_Clear_1);
   pragma Atomic (GPIO_Output_Clear_1);

   -- GPLEV register repreesentation type --

   type GPIO_Level_Register_Type is array (0 .. 31) of Boolean;
   pragma Pack (GPIO_Level_Register_Type);
   for GPIO_Level_Register_Type'Size use 32;

   -- GPLEVn addresses --
   GPIO_Pin_Level_0_Address : constant Address := GPIO_Base_Address + 16#34#;
   GPIO_Pin_Level_1_Address : constant Address := GPIO_Base_Address + 16#38#;

   -- GPLEVn --
   GPIO_Pin_Level_0 : GPIO_Level_Register_Type;
   for GPIO_Pin_Level_0'Address use GPIO_Pin_Level_0_Address;
   pragma Volatile (GPIO_Pin_Level_0);
   pragma Atomic (GPIO_Pin_Level_0);

   GPIO_Pin_Level_1 : GPIO_Level_Register_Type;
   for GPIO_Pin_Level_1'Address use GPIO_Pin_Level_1_Address;
   pragma Volatile (GPIO_Pin_Level_1);
   pragma Atomic (GPIO_Pin_Level_1);

   -- GPPUD --

   GPIO_GPPUD_Address : constant Address := GPIO_Base_Address + 16#94#;

   type GPIO_GPPUD_RESERVED is (RESERVED);
   for GPIO_GPPUD_RESERVED use (RESERVED => 0);
   for GPIO_GPPUD_RESERVED'Size use 30;

   type GPIO_GPPUD_Register_Type is record
      PUD      : GPIO_Pin_Pull_Up_Down;
      RESERVED : GPIO_GPPUD_RESERVED;
   end record;
   for GPIO_GPPUD_Register_Type use record
      PUD      at 0 range  0 .. 1;
      RESERVED at 0 range  2 .. 31;
   end record;
   for GPIO_GPPUD_Register_Type'Size use 32;

   GPIO_GPPUD : GPIO_GPPUD_Register_Type;
   for GPIO_GPPUD'Address use GPIO_GPPUD_Address;
   pragma Volatile (GPIO_GPPUD);
   pragma Atomic (GPIO_GPPUD);

   --  GPPUDCLKn --

   GPIO_GPPUDCLK0_Address : constant Address := GPIO_Base_Address + 16#98#;
   GPIO_GPPUDCLK1_Address : constant Address := GPIO_Base_Address + 16#9C#;

   type GPIO_GPPUDCLK0_Register_Type is range 0 .. 2**32 - 1;
   for GPIO_GPPUDCLK0_Register_Type'Size use 32;

   GPIO_GPPUDCLK0 : GPIO_GPPUDCLK0_Register_Type;
   for GPIO_GPPUDCLK0'Address use GPIO_GPPUDCLK0_Address;
   pragma Volatile (GPIO_GPPUDCLK0);
   pragma Atomic (GPIO_GPPUDCLK0);

   type GPIO_GPPUDCLK1_PUDCLK1 is array (32 .. 53) of Boolean;
   pragma Pack (GPIO_GPPUDCLK1_PUDCLK1);
   for GPIO_GPPUDCLK1_PUDCLK1'Size use 22;

   type GPIO_GPPUDCLK1_RESERVED is (RESERVED);
   for GPIO_GPPUDCLK1_RESERVED use (RESERVED => 0);
   for GPIO_GPPUDCLK1_RESERVED'Size use 10;

   type GPIO_GPPUDCLK1_Register_Type is record
      PUDCLK1  : GPIO_GPPUDCLK1_PUDCLK1;
      RESERVED : GPIO_GPPUDCLK1_RESERVED;
   end record;
   for GPIO_GPPUDCLK1_Register_Type use record
      PUDCLK1  at 0 range  0  .. 21;
      RESERVED at 0 range  22 .. 31;
   end record;
   for GPIO_GPPUDCLK1_Register_Type'Size use 32;

   GPIO_GPPUDCLK1 : GPIO_GPPUDCLK1_Register_Type;
   for GPIO_GPPUDCLK1'Address use GPIO_GPPUDCLK1_Address;
   pragma Volatile (GPIO_GPPUDCLK1);
   pragma Atomic (GPIO_GPPUDCLK1);

   ------------------------
   --  internal routines --
   ------------------------

   --  Control structures --

   In_Use : array (GPIO_Number) of Boolean := (others => False);
   --  control whether GPIO is in use

   -- Mask for SET/CLEAR registers
   Set_Clear_Mask : array (GPIO_Number) of GPIO_Set_Clear_Register_Type :=
     (0  =>                            2#1#,
      1  =>                           2#10#,
      2  =>                          2#100#,
      3  =>                         2#1000#,
      4  =>                        2#10000#,
      5  =>                       2#100000#,
      6  =>                      2#1000000#,
      7  =>                     2#10000000#,
      8  =>                    2#100000000#,
      9  =>                   2#1000000000#,
      10 =>                  2#10000000000#,
      11 =>                 2#100000000000#,
      12 =>                2#1000000000000#,
      13 =>               2#10000000000000#,
      14 =>              2#100000000000000#,
      15 =>             2#1000000000000000#,
      16 =>            2#10000000000000000#,
      17 =>           2#100000000000000000#,
      18 =>          2#1000000000000000000#,
      19 =>         2#10000000000000000000#,
      20 =>        2#100000000000000000000#,
      21 =>       2#1000000000000000000000#,
      22 =>      2#10000000000000000000000#,
      23 =>     2#100000000000000000000000#,
      24 =>    2#1000000000000000000000000#,
      25 =>   2#10000000000000000000000000#,
      26 =>  2#100000000000000000000000000#,
      27 => 2#1000000000000000000000000000#);

   -- Mask for GPPUDCLK0 register
   Set_GPPUDCLK0_Mask : array (0 .. 31) of GPIO_GPPUDCLK0_Register_Type :=
     (0  =>                                2#1#,
      1  =>                               2#10#,
      2  =>                              2#100#,
      3  =>                             2#1000#,
      4  =>                            2#10000#,
      5  =>                           2#100000#,
      6  =>                          2#1000000#,
      7  =>                         2#10000000#,
      8  =>                        2#100000000#,
      9  =>                       2#1000000000#,
      10 =>                      2#10000000000#,
      11 =>                     2#100000000000#,
      12 =>                    2#1000000000000#,
      13 =>                   2#10000000000000#,
      14 =>                  2#100000000000000#,
      15 =>                 2#1000000000000000#,
      16 =>                2#10000000000000000#,
      17 =>               2#100000000000000000#,
      18 =>              2#1000000000000000000#,
      19 =>             2#10000000000000000000#,
      20 =>            2#100000000000000000000#,
      21 =>           2#1000000000000000000000#,
      22 =>          2#10000000000000000000000#,
      23 =>         2#100000000000000000000000#,
      24 =>        2#1000000000000000000000000#,
      25 =>       2#10000000000000000000000000#,
      26 =>      2#100000000000000000000000000#,
      27 =>     2#1000000000000000000000000000#,
      28 =>    2#10000000000000000000000000000#,
      29 =>   2#100000000000000000000000000000#,
      30 =>  2#1000000000000000000000000000000#,
      31 => 2#10000000000000000000000000000000#);

   procedure Free is
     new Ada.Unchecked_Deallocation (Pin_Node, Pin_Node_Access);

   procedure Reserve_Pin (Number : GPIO_Number);

   procedure Set_Pin_Mode (Number : GPIO_Number; Mode : GPIO_FSEL_Mode);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Pin) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter + 1;
      end if;
   end Adjust;

   ------------
   -- Create --
   ------------

   function Create
     (Number : GPIO_Number)
      return Output_Pin is
   begin
      Reserve_Pin (Number);
      Set_Pin_Mode (Number, Output);
      GPIO_Output_Clear_0 := Set_Clear_Mask (Number);

      return Result : Output_Pin do
         Result.Node := new Pin_Node'
           (Number   => Number,
            Counter  => 1,
            Is_High  => False,
            GPIO_Pin => Number);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Number : GPIO_Number)
      return Input_Pin is
   begin
      Reserve_Pin (Number);
      Set_Pin_Mode (Number, Input);

      return Result : Input_Pin do
         Result.Node := new Pin_Node'
           (Number   => Number,
            Counter  => 1,
            Is_High  => False,
            GPIO_Pin => Number);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Number : GPIO_Number;
      Mode   : GPIO_FSEL_Mode)
      return Alternate_Pin is
   begin
      Reserve_Pin (Number);
      Set_Pin_Mode (Number, Mode);
      GPIO_Output_Clear_0 := Set_Clear_Mask (Number);

      return Result : Alternate_Pin do
         Result.Node := new Pin_Node'
           (Number   => Number,
            Counter  => 1,
            Is_High  => False,
            GPIO_Pin => Number);
      end return;
   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Pin) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter - 1;
         if Self.Node.Counter = 0 then
            In_Use (Self.Node.Number) := False;
            Set_Pin_Mode (Self.Node.Number, Input);
            Free (Self.Node);
         end if;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Output_Pin) is
   begin
      if Self.Node /= null
        and then Self.Node.Counter = 1
      then
         Self.Low;
      end if;

      Finalize (Pin (Self));
   end Finalize;

   --------
   -- Id --
   --------

   function Id (Self : Pin) return GPIO_Number is
   begin
      return Self.Node.GPIO_Pin;
   end Id;

   ----------
   -- High --
   ----------

   procedure High (Self : in out Output_Pin) is
   begin
      if not Self.Node.Is_High then
         GPIO_Output_Set_0 := Set_Clear_Mask (Self.Node.Number);
         Self.Node.Is_High := True;
      end if;
   end High;

   -------------
   -- Is_High --
   -------------

   function Is_High (Self : Input_Pin) return Boolean is
      Register : GPIO_Level_Register_Type := GPIO_Pin_Level_0;
   begin
      return Register (Integer (Self.Node.Number));
   end Is_High;

   ------------
   -- Is_Low --
   ------------

   function Is_Low (Self : Input_Pin) return Boolean is
   begin
      return not Self.Is_High;
   end Is_Low;

   ---------
   -- Low --
   ---------

   procedure Low (Self : in out Output_Pin) is
   begin
      if Self.Node.Is_High then
         GPIO_Output_Clear_0 := Set_Clear_Mask (Self.Node.Number);
         Self.Node.Is_High := False;
      end if;
   end Low;

   -----------------
   -- Reserve_Pin --
   -----------------

   procedure Reserve_Pin (Number : GPIO_Number) is
   begin
      if In_Use (Number) then
         raise Program_Error with "GPIO" & Number'Img & " is already in use";
      end if;

      In_Use (Number) := True;
   end Reserve_Pin;

   ------------------
   -- Set_Pin_Mode --
   ------------------

   procedure Set_Pin_Mode (Number : GPIO_Number; Mode : GPIO_FSEL_Mode) is
      Num      : constant Natural := Natural (Number / 10);
      Register : GPIO_Select_Register_Type;
   begin
      case Num is
         when 0 =>
            Register := GPIO_Select_0;
         when 1 =>
            Register := GPIO_Select_1;
         when 2 =>
            Register := GPIO_Select_2;
         when others =>
            raise Constraint_Error;
      end case;

      case Number rem 10 is
         when 0 =>
            Register.FSEL0 := Mode;
         when 1 =>
            Register.FSEL1 := Mode;
         when 2 =>
            Register.FSEL2 := Mode;
         when 3 =>
            Register.FSEL3 := Mode;
         when 4 =>
            Register.FSEL4 := Mode;
         when 5 =>
            Register.FSEL5 := Mode;
         when 6 =>
            Register.FSEL6 := Mode;
         when 7 =>
            Register.FSEL7 := Mode;
         when 8 =>
            Register.FSEL8 := Mode;
         when 9 =>
            Register.FSEL9 := Mode;
         when others =>
            raise Constraint_Error;
      end case;

      case Num is
         when 0 =>
            GPIO_Select_0 := Register;
         when 1 =>
            GPIO_Select_1 := Register;
         when 2 =>
            GPIO_Select_2 := Register;
         when others =>
            raise Constraint_Error;
      end case;
   end Set_Pin_Mode;

   ----------------------
   -- Set_Pull_Up_Down --
   ----------------------

   procedure Set_Pull_Up_Down
     (Self : Input_Pin;
      Mode : GPIO_Pin_Pull_Up_Down)
   is
      Id : constant Integer := Integer (Self.Node.Number);
   begin
      GPIO_GPPUD := (PUD => Mode, RESERVED => RESERVED);
      delay (0.00001);

      if Self.Node.Number < 32 then
         GPIO_GPPUDCLK0 := Set_GPPUDCLK0_Mask (Id);
      else
         declare
            V : GPIO_GPPUDCLK1_PUDCLK1 := (others => False);
         begin
            V (Id) := True;
            GPIO_GPPUDCLK1 := (PUDCLK1 => V, RESERVED => RESERVED);
         end;
      end if;
      delay (0.00001);

      GPIO_GPPUD := (PUD => Off, RESERVED => RESERVED);

      if Self.Node.Number < 32 then
         GPIO_GPPUDCLK0 := 0;
      else
         declare
            V : GPIO_GPPUDCLK1_PUDCLK1 := (others => False);
         begin
            GPIO_GPPUDCLK1 := (PUDCLK1 => V, RESERVED => RESERVED);
         end;
      end if;
   end Set_Pull_Up_Down;

   --------------
   -- Wait_Low --
   --------------

   function Wait_Low (Self : Input_Pin; Timaout : Duration) return Boolean is
      T : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      loop
         if Self.Is_Low then
            return True;
         end if;

         if Ada.Calendar.Clock - T > Timaout then
            return False;
         end if;
      end loop;
   end Wait_Low;

end GPIO;
