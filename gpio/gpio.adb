
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;     use System, System.Storage_Elements;
with Interfaces.C;                use Interfaces.C;

package body GPIO is

   --  GPIO Mode type (FSEL) --

   type GPIO_FSEL_Mode is (Input, Output);
   for GPIO_FSEL_Mode use (Input => 0, Output => 1);
   for GPIO_FSEL_Mode'Size use 3;

   -- GPFSEL register repreesentation type --

   type GPIO_Select_Register_Type is record
      FSEL0 : GPIO_FSEL_Mode;
      FSEL1 : GPIO_FSEL_Mode;
      FSEL2 : GPIO_FSEL_Mode;
      FSEL3 : GPIO_FSEL_Mode;
      FSEL4 : GPIO_FSEL_Mode;
      FSEL5 : GPIO_FSEL_Mode;
      FSEL6 : GPIO_FSEL_Mode;
      FSEL7 : GPIO_FSEL_Mode;
      FSEL8 : GPIO_FSEL_Mode;
      FSEL9 : GPIO_FSEL_Mode;
   end record;
   for GPIO_Select_Register_Type use record
      FSEL0 at 0 range  0 .. 2;
      FSEL1 at 0 range  3 .. 5;
      FSEL2 at 0 range  6 .. 8;
      FSEL3 at 0 range  9 .. 11;
      FSEL4 at 0 range 12 .. 14;
      FSEL5 at 0 range 15 .. 17;
      FSEL6 at 0 range 18 .. 20;
      FSEL7 at 0 range 21 .. 23;
      FSEL8 at 0 range 24 .. 26;
      FSEL9 at 0 range 27 .. 29;
   end record;
   for GPIO_Select_Register_Type'Size use 32;
   pragma Volatile (GPIO_Select_Register_Type);
   pragma Atomic (GPIO_Select_Register_Type);

   -- GPFSELn addresses --

   GPIO_Select_0_Address : constant Address := To_Address (16#3F20_0000#);
   GPIO_Select_1_Address : constant Address := To_Address (16#3F20_0004#);
   GPIO_Select_2_Address : constant Address := To_Address (16#3F20_0008#);

   -- GPFSELn --

   GPIO_Select_0 : aliased GPIO_Select_Register_Type;
   for GPIO_Select_0'Address use GPIO_Select_0_Address;

   GPIO_Select_1 : aliased GPIO_Select_Register_Type;
   for GPIO_Select_1'Address use GPIO_Select_1_Address;

   GPIO_Select_2 : aliased GPIO_Select_Register_Type;
   for GPIO_Select_2'Address use GPIO_Select_2_Address;

   -- GPSET and GPCLR register repreesentation type --

   type GPIO_Set_Clear_Register_Type is range 0 .. 2**32 - 1;
   for GPIO_Set_Clear_Register_Type'Size use 32;
   pragma Volatile (GPIO_Set_Clear_Register_Type);

   -- GPSETn addresses --
   GPIO_Output_Set_0_Address : constant Address := To_Address (16#3F20_001C#);
   GPIO_Output_Set_1_Address : constant Address := To_Address (16#3F20_0020#);

   -- GPSETn --
   GPIO_Output_Set_0 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Set_0'Address use GPIO_Output_Set_0_Address;
   GPIO_Output_Set_1 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Set_1'Address use GPIO_Output_Set_1_Address;

   -- GPCLRn addresses --
   GPIO_Output_Clear_0_Address : constant Address :=
     To_Address (16#3F20_0028#);
   GPIO_Output_Clear_1_Address : constant Address :=
     To_Address (16#3F20_002C#);

   -- GPCLRn --
   GPIO_Output_Clear_0 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Clear_0'Address use GPIO_Output_Clear_0_Address;
   GPIO_Output_Clear_1 : GPIO_Set_Clear_Register_Type;
   for GPIO_Output_Clear_1'Address use GPIO_Output_Clear_1_Address;

   -- GPLEV register repreesentation type --

   type GPIO_Level_Register_Type is array (0 .. 31) of Boolean;
   pragma Pack (GPIO_Level_Register_Type);
   pragma Volatile (GPIO_Level_Register_Type);
   for GPIO_Level_Register_Type'Size use 32;

   -- GPLEVn addresses --
   GPIO_Pin_Level_0_Address : constant Address := To_Address (16#3F20_0034#);
   GPIO_Pin_Level_1_Address : constant Address := To_Address (16#3F20_0038#);

   -- GPLEVn --
   GPIO_Pin_Level_0 : GPIO_Level_Register_Type;
   for GPIO_Pin_Level_0'Address use GPIO_Pin_Level_0_Address;

   GPIO_Pin_Level_1 : GPIO_Level_Register_Type;
   for GPIO_Pin_Level_1'Address use GPIO_Pin_Level_1_Address;

   ------------------------
   --  internal routines --
   ------------------------

   --  Control structures --

   In_Use : array (GPIO_Number) of Boolean := (others => False);
   --  control whether GPIO is in use

   -- Mask for SET/CLEAR registers
   Set_Clear_Mask : array (GPIO_Number) of GPIO_Set_Clear_Register_Type :=
     (0  => 2#1#,
      1  => 2#10#,
      2  => 2#100#,
      3  => 2#1000#,
      4  => 2#10000#,
      5  => 2#100000#,
      6  => 2#1000000#,
      7  => 2#10000000#,
      8  => 2#100000000#,
      9  => 2#1000000000#,
      10 => 2#10000000000#,
      11 => 2#100000000000#,
      12 => 2#1000000000000#,
      13 => 2#10000000000000#,
      14 => 2#100000000000000#,
      15 => 2#1000000000000000#,
      16 => 2#10000000000000000#,
      17 => 2#100000000000000000#,
      18 => 2#1000000000000000000#,
      19 => 2#10000000000000000000#,
      20 => 2#100000000000000000000#,
      21 => 2#1000000000000000000000#,
      22 => 2#10000000000000000000000#,
      23 => 2#100000000000000000000000#,
      24 => 2#1000000000000000000000000#,
      25 => 2#10000000000000000000000000#,
      26 => 2#100000000000000000000000000#,
      27 => 2#1000000000000000000000000000#);

   type GPIO_Select_Register_Type_Access is
     access all GPIO_Select_Register_Type;

   procedure Free is
     new Ada.Unchecked_Deallocation (Pin_Node, Pin_Node_Access);

   procedure Initialize_Pin (Number : GPIO_Number; Mode : GPIO_FSEL_Mode);

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
      if In_Use (Number) then
         raise Program_Error with "GPIO is already in use";
      end if;

      In_Use (Number) := True;
      Initialize_Pin (Number, Output);
      GPIO_Output_Clear_0 := Set_Clear_Mask (Number);

      return Result : Output_Pin do
         Result.Node := new Pin_Node'
           (Number => Number, Counter => 1,
            Is_High => False, GPIO_Pin => Number);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Number : GPIO_Number)
      return Input_Pin is
   begin
      if In_Use (Number) then
         raise Program_Error with "GPIO is already in use";
      end if;

      In_Use (Number) := True;
      Initialize_Pin (Number, Input);

      return Result : Input_Pin do
         Result.Node := new Pin_Node'
           (Number => Number, Counter => 1,
            Is_High => False, GPIO_Pin => Number);
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
   begin
      return GPIO_Pin_Level_0 (Integer (Self.Node.Number));
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

   --------------------
   -- Initialize_Pin --
   --------------------

   procedure Initialize_Pin (Number : GPIO_Number; Mode : GPIO_FSEL_Mode) is
      Register : GPIO_Select_Register_Type_Access;
   begin
      case Natural (Number / 10) is
         when 0 =>
            Register := GPIO_Select_0'Access;
         when 1 =>
            Register := GPIO_Select_1'Access;
         when 2 =>
            Register := GPIO_Select_2'Access;
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
   end Initialize_Pin;

   --  int map_pheriferal ()
   function Map_Pheriferal return Interfaces.C.int;
   pragma Import (C, Map_Pheriferal, "map_pheriferal");

   Res : Interfaces.C.int;

begin
   Res := Map_Pheriferal;

   if Res = -1 then
      raise Constraint_Error with "Can't open device";
   elsif Res = -2 then
      raise Constraint_Error with "Can't map GPIO memory";
   end if;
end GPIO;
