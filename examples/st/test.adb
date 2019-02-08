
--  This example shows how to read value from system timer --

with Ada.Text_IO;
with Ada.Exceptions;

with System_Timer;

procedure Test is
begin
   Ada.Text_IO.Put_line ("Starting");

   for I in 1 .. 2 loop
      Ada.Text_IO.Put_Line
        ("ST value :" &
           System_Timer.System_Timer_Value_Type'Image (System_Timer.Get));
      delay 0.5;
   end loop;

exception
   when E: others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Test;
