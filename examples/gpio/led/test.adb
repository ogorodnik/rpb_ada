
-- This example shows how to control (green) led connected to GPIO(25) --

with Ada.Text_IO;
with Ada.Exceptions;

with Sensors.Digital.Output.Led; use Sensors.Digital.Output.Led;

procedure Test is
   Green : Led := Create (22);
begin
   Ada.Text_IO.Put_line ("Starting");

   Green.On;
   delay 1.0;
   Green.Off;

exception
   when E: others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Test;
