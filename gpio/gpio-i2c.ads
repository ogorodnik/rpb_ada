--------------------------------------
--  Raspberry Pi3 ( bcm2837B0 ) I2C --
--------------------------------------

--  GPIO Pins 0, 1, 2, 3 are used for I2C protocol

with GPIO.Types; use GPIO.Types;

package GPIO.I2C is

   type Slave_Address_7Bit is range 0 .. 2**6 - 1;
   for Slave_Address_7Bit'Size use 6;
   --  Address without first WR bit

   type I2C_Clock_Divider is range 0 .. 2**16 - 1;
   for I2C_Clock_Divider'Size use 16;

   type Unsigned_Integer_8_Array is
     array (Positive range <>) of Unsigned_Integer_8;

   -- I2C --

   type I2C is abstract tagged private;

   type Send_Result_Kind is
     (No_Error, Wrong_Address, Clock_Stretching, Data_Left);

   type Send_Result (Kind : Send_Result_Kind := No_Error) is record
      case Kind is
         when Data_Left =>
            Last_Written_Index : Positive;
         when others =>
            null;
      end case;
   end record;

   function Get
     (Self    : I2C;
      Address : Slave_Address_7Bit;
      Divider : I2C_Clock_Divider)
      return Unsigned_Integer_8;

   function Send
     (Self    : I2C;
      Address : Slave_Address_7Bit;
      Divider : I2C_Clock_Divider;
      Data    : Unsigned_Integer_8_Array)
      return Send_Result;

   type I2C_BSC0 is new I2C with private;
   function Create return I2C_BSC0;

   type I2C_BSC1 is new I2C with private;
   function Create return I2C_BSC1;

private

   type BSC_Number is (BSC0, BSC1);  --  BSC2 is used dedicated with the HDMI

   type Slave_Address_Write_Read_Flag is (Write, Read);
   for Slave_Address_Write_Read_Flag use
     (Write => 0,
      Read  => 1);
   for Slave_Address_Write_Read_Flag'Size use 1;

   type Slave_Address_7Bit_WR is record
      WR   : Slave_Address_Write_Read_Flag;
      ADDR : Slave_Address_7Bit;
   end record;
   for Slave_Address_7Bit_WR use record
      WR   at 0 range 0 .. 0;
      ADDR at 0 range 1 .. 6;
   end record;
   for Slave_Address_7Bit_WR'Size use 7;

   type I2C_Node is record
      Counter : Natural := 0;
      Number  : BSC_Number;
      Address : Slave_Address_7Bit_WR;
      Timeout : Duration;
   end record;
   type I2C_Node_Access is access all I2C_Node;

   type I2C is new Ada.Finalization.Controlled with record
      Node : I2C_Node_Access;
   end record;

   overriding procedure Adjust   (Self : in out I2C);
   overriding procedure Finalize (Self : in out I2C);

   procedure Set_Slave_Address
     (Self    : I2C;
      Address : Slave_Address_7Bit_WR;
      Divider : I2C_Clock_Divider);

   type I2C_BSC0 is new I2C with null record;
   type I2C_BSC1 is new I2C with null record;

end GPIO.I2C;
