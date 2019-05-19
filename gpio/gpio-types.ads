----------------------------------
--  Raspberry Pi3 ( bcm2837B0 ) --
----------------------------------

package GPIO.Types is

   type Bit is range 0 .. 1;
   for Bit'Size use 1;

   type Unsigned_Integer_8 is range 0 .. 2**8 - 1;
   for Unsigned_Integer_8'Size use 8;

   type Unsigned_Integer_16 is range 0 .. 2**16 - 1;
   for Unsigned_Integer_16'Size use 16;

   type Unsigned_Integer_32 is mod 2**32;
   for Unsigned_Integer_32'Size use 32;

end GPIO.Types;
