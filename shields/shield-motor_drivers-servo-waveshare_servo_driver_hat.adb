
with Ada.Unchecked_Conversion;

with GPIO.Types; use GPIO.Types;

package body Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT is

   use GPIO.I2C;

   -- Register definitions --

   MODE1        : constant := 16#00#;
   MODE2        : constant := 16#01#;
   SUBADR1      : constant := 16#02#;
   SUBADR2      : constant := 16#03#;
   SUBADR3      : constant := 16#04#;
   ALLCALLADR   : constant := 16#05#;

   LED0_ON_L    : constant := 16#06#;
   LED0_ON_H    : constant := 16#07#;
   LED0_OFF_L   : constant := 16#08#;
   LED0_OFF_H   : constant := 16#09#;
   LED1_ON_L    : constant := 16#0A#;
   LED1_ON_H    : constant := 16#0B#;
   LED1_OFF_L   : constant := 16#0C#;
   LED1_OFF_H   : constant := 16#0D#;
   LED2_ON_L    : constant := 16#0E#;
   LED2_ON_H    : constant := 16#0F#;
   LED2_OFF_L   : constant := 16#10#;
   LED2_OFF_H   : constant := 16#11#;
   LED3_ON_L    : constant := 16#12#;
   LED3_ON_H    : constant := 16#13#;
   LED3_OFF_L   : constant := 16#14#;
   LED3_OFF_H   : constant := 16#15#;
   LED4_ON_L    : constant := 16#16#;
   LED4_ON_H    : constant := 16#17#;
   LED4_OFF_L   : constant := 16#18#;
   LED4_OFF_H   : constant := 16#19#;
   LED5_ON_L    : constant := 16#1A#;
   LED5_ON_H    : constant := 16#1B#;
   LED5_OFF_L   : constant := 16#1C#;
   LED5_OFF_H   : constant := 16#1D#;
   LED6_ON_L    : constant := 16#1E#;
   LED6_ON_H    : constant := 16#1F#;
   LED6_OFF_L   : constant := 16#20#;
   LED6_OFF_H   : constant := 16#21#;
   LED7_ON_L    : constant := 16#22#;
   LED7_ON_H    : constant := 16#23#;
   LED7_OFF_L   : constant := 16#24#;
   LED7_OFF_H   : constant := 16#25#;
   LED8_ON_L    : constant := 16#26#;
   LED8_ON_H    : constant := 16#27#;
   LED8_OFF_L   : constant := 16#28#;
   LED8_OFF_H   : constant := 16#29#;
   LED9_ON_L    : constant := 16#2A#;
   LED9_ON_H    : constant := 16#2B#;
   LED9_OFF_L   : constant := 16#2C#;
   LED9_OFF_H   : constant := 16#2D#;
   LED10_ON_L   : constant := 16#2E#;
   LED10_ON_H   : constant := 16#2F#;
   LED10_OFF_L  : constant := 16#30#;
   LED10_OFF_H  : constant := 16#31#;
   LED11_ON_L   : constant := 16#32#;
   LED11_ON_H   : constant := 16#33#;
   LED11_OFF_L  : constant := 16#34#;
   LED11_OFF_H  : constant := 16#35#;
   LED12_ON_L   : constant := 16#36#;
   LED12_ON_H   : constant := 16#37#;
   LED12_OFF_L  : constant := 16#38#;
   LED12_OFF_H  : constant := 16#39#;
   LED13_ON_L   : constant := 16#3A#;
   LED13_ON_H   : constant := 16#3B#;
   LED13_OFF_L  : constant := 16#3C#;
   LED13_OFF_H  : constant := 16#3D#;
   LED14_ON_L   : constant := 16#3E#;
   LED14_ON_H   : constant := 16#3F#;
   LED14_OFF_L  : constant := 16#40#;
   LED14_OFF_H  : constant := 16#41#;
   LED15_ON_L   : constant := 16#42#;
   LED15_ON_H   : constant := 16#43#;
   LED15_OFF_L  : constant := 16#44#;
   LED15_OFF_H  : constant := 16#45#;

   ALL_LED_ON_L  : constant := 16#FA#;
   ALL_LED_ON_H  : constant := 16#FB#;
   ALL_LED_OFF_L : constant := 16#FA#;
   ALL_LED_OFF_H : constant := 16#FB#;
   PRE_SCALE     : constant := 16#FE#;
   TestMode      : constant := 16#FF#;

   type Mode1_Register_Type is record
      ALLCALL  : Bit;
      SUB3     : Bit;
      SUB2     : Bit;
      SUB1     : Bit;
      SLEEP    : Bit;
      AI       : Bit;
      EXTCLK   : Bit;
      RESTART  : Bit;
   end record;
   for Mode1_Register_Type use record
      ALLCALL  at 0 range  0 .. 0;
      SUB3     at 0 range  1 .. 1;
      SUB2     at 0 range  2 .. 2;
      SUB1     at 0 range  3 .. 3;
      SLEEP    at 0 range  4 .. 4;
      AI       at 0 range  5 .. 5;
      EXTCLK   at 0 range  6 .. 6;
      RESTART  at 0 range  7 .. 7;
   end record;
   for Mode1_Register_Type'Size use 8;

   type LED_H_Value is range 0 .. 2**4 - 1;
   for LED_H_Value'Size use 4;

   type LED_H_Reserved is (Zero);
   for LED_H_Reserved use (Zero => 0);
   for LED_H_Reserved'Size use 3;

   type LED_H_Register_Type is record
      VALUE    : LED_H_Value;
      FULL     : Bit;
      RESERVED : LED_H_Reserved;
   end record;
   for LED_H_Register_Type use record
      VALUE    at 0 range  0 .. 3;
      FULL     at 0 range  4 .. 4;
      RESERVED at 0 range  5 .. 7;
   end record;
   for LED_H_Register_Type'Size use 8;

   type LED_Register_Type is record
      L : Unsigned_Integer_8;
      H : LED_H_Register_Type;
   end record;
   for LED_Register_Type'Size use 16;

   --  Internal --

   function Convert is
     new Ada.Unchecked_Conversion (Mode1_Register_Type, Unsigned_Integer_8);
   function Convert is
     new Ada.Unchecked_Conversion (Unsigned_Integer_8, Mode1_Register_Type);
   function Convert is
     new Ada.Unchecked_Conversion (LED_H_Register_Type, Unsigned_Integer_8);
   function Convert is
     new Ada.Unchecked_Conversion (Unsigned_Integer_16, LED_Register_Type);

   procedure Set_PWM_Frequency
     (Self : Servo_Driver_HAT;
      Hz   : Frequency);

   ------------
   -- Create --
   ------------

   function Create
     (I2C          : GPIO.I2C.I2C_BSC1;
      Addr         : Address;
      Hz_Frequency : Frequency)
      return Servo_Driver_HAT
   is
      A      : Slave_Address_7Bit := 16#20#; -- default 16#40# without RW bit
      Result : Servo_Driver_HAT;
      Dummy  : Send_Result;
   begin
      if Addr(A0) then
         A := A + 16#1#;
      end if;

      if Addr(A1) then
         A := A + 16#2#;
      end if;

      if Addr(A3) then
         A := A + 16#4#;
      end if;

      if Addr(A4) then
         A := A + 16#8#;
      end if;

      Result.Node := new Servo_Driver_HAT_Node'
        (Counter => 1,
         I2C     => I2C,
         Addr    => A,
         Hz      => Hz_Frequency);

      -- reset
      Dummy := I2C.Send
        (A,
         626,
         Unsigned_Integer_8_Array'
           (Mode1, Convert (Mode1_Register_Type'(others => 0))));

      Set_PWM_Frequency (Result, Hz_Frequency);

      return Result;
   end Create;

   -------------------
   -- Get_Frequency --
   -------------------

   overriding function Get_Frequency
     (Self : Servo_Driver_HAT)
      return Frequency is
   begin
      return Servo_Driver_HAT_Node_Access (Self.Node).Hz;
   end Get_Frequency;

   -----------------------
   -- Set_PWM_Frequency --
   -----------------------

   procedure Set_PWM_Frequency
     (Self : Servo_Driver_HAT;
      Hz   : Frequency)
   is
      V       : Unsigned_Integer_8;
      Node    : Servo_Driver_HAT_Node_Access :=
        Servo_Driver_HAT_Node_Access (Self.Node);
      Dummy   : Send_Result;
      Oldmode : Mode1_Register_Type;
      Newmode : Mode1_Register_Type;

   begin
      V := Unsigned_Integer_8 (25000000.0 / 4096.0 / Float (Hz) - 0.5);

      Dummy := Node.I2C.Send
        (Node.Addr, 626, Unsigned_Integer_8_Array'(1 => Mode1));
      Oldmode := Convert (Node.I2C.Get (Node.Addr, 626));

      Newmode := Oldmode;
      Newmode.RESTART := 0;
      Newmode.SLEEP   := 1;

      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(Mode1, Convert (Newmode)));

      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(PRE_SCALE, V));

      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(Mode1, Convert (Oldmode)));

      delay 0.005;

      Oldmode.RESTART := 1;
      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(Mode1, Convert (Oldmode)));
   end Set_PWM_Frequency;

   ------------------
   -- Rotate_Servo --
   ------------------

   overriding procedure Rotate_Servo
     (Self   : Servo_Driver_HAT;
      Chanel : Motor_Chanel_Number;
      On     : PWM_Value;
      Off    : PWM_Value)
   is
      Node  : Servo_Driver_HAT_Node_Access :=
        Servo_Driver_HAT_Node_Access (Self.Node);
      L, H  : Unsigned_Integer_16;
      R     : LED_Register_Type;
      Dummy : Send_Result;
      C     : Unsigned_Integer_8;
   begin
      C := 16#06# + Unsigned_Integer_8 (Chanel) * 4;
      L := Unsigned_Integer_16 (On)  * 4096 / 20000;
      H := Unsigned_Integer_16 (Off) * 4096 / 20000;

      R := Convert (L);
      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(C, R.L));
      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(C + 1, Convert (R.H)));

      R := Convert (H);
      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(C + 2, R.L));
      Dummy := Node.I2C.Send
        (Node.Addr,
         626,
         Unsigned_Integer_8_Array'(C + 3, Convert (R.H)));
   end Rotate_Servo;

end Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;
