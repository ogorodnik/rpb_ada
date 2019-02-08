
-----------------------------------------------
--  Raspberry Pi3 ( bcm2837B0 ) System Timer --
-----------------------------------------------

package System_Timer is

   type System_Timer_Value_Type is mod 2**64;
   for System_Timer_Value_Type'Size use 64;

   function Get return System_Timer_Value_Type;
   --  Get system timer value

end System_Timer;
