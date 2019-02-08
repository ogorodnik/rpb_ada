
package body Peripherals is

--   Peripherals_Base_Address : constant Address := To_Address (16#3F00_0000#);

    --  int map_all ()
   function Map_All return Address;
   pragma Import (C, Map_All, "map_all");

   Base : Address := Null_Address;

   function Base_Address return Address is
   begin
      return Base;
   end Base_Address;

begin
   Base := Map_All;
   if Base = Null_Address then
      raise Program_Error with "Can't map memory";
   end if;
end Peripherals;
