
-- This example shows how read analog data as digital from
-- WAVESHARE Raspberry Pi High-Precision AD/DA Expansion Board,
-- from buitin potentiomener.

with Ada.Text_IO;
with Ada.Exceptions;

with Shield.Analog.WAVESHARE_DA_AD_11010;
use  Shield.Analog.WAVESHARE_DA_AD_11010;

with Sensors.Analog.Input.Potentiometer;
use  Sensors.Analog.Input.Potentiometer;

procedure Test is
   Board  : aliased DA_AD_Board;
   Sensor : Potentiometer;

begin
   Board  := Create (Mode => AD);
   Ada.Text_IO.Put_Line ("WAVESHARE Chip ID:" & Board.Chip_Id'Img);

   Sensor := Create (Board'Unchecked_Access, 0);
   for I in 1 .. 5 loop
      Ada.Text_IO.Put_Line
        ("Builtin potentiometer on chanel 0:" & Sensor.Value'Img);
   end loop;

exception
   when E: others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Test;
