
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;

with Peripherals;

package body System_Timer is

   ST_Base_Address : constant Address :=
     Peripherals.Base_Address + 16#3000#;

   ST_CLO_Address : constant Address := ST_Base_Address + 16#4#;
   ST_CHI_Address : constant Address := ST_Base_Address + 16#8#;

   type ST_CNT_Register_Type is mod 2**32;
   for ST_CNT_Register_Type'Size use 32;

   ST_CLO : ST_CNT_Register_Type;
   for ST_CLO'Address use ST_CLO_Address;
   pragma Volatile (ST_CLO);
   pragma Atomic (ST_CLO);

   ST_CHI : ST_CNT_Register_Type;
   for ST_CHI'Address use ST_CHI_Address;
   pragma Volatile (ST_CHI);
   pragma Atomic (ST_CHI);

   ---------
   -- Get --
   ---------

   function Get return System_Timer_Value_Type
   is
      L, H, R : ST_CNT_Register_Type;
   begin

      H := ST_CHI;
      L := ST_CLO;
      R := ST_CHI;

      if R = H then
         return System_Timer_Value_Type
           (Shift_Left (Unsigned_64 (H), 32) + Unsigned_64 (L));
      else
         return System_Timer_Value_Type
           (Shift_Left (Unsigned_64( R), 32) + Unsigned_64 (ST_CLO));
      end if;
   end Get;

end System_Timer;
