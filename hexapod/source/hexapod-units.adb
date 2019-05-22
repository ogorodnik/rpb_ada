with Ada.Numerics.Generic_Elementary_Functions;

with Motors.Servo.Angle.MG995;

with Hexapod.Programs;

package body Hexapod.Units is

   X : constant := 75;   --  X of an "side leg"
   Y : constant := 40;   --  Y of an "side leg"
   Z : constant := 4;    --  Z for all legs
   M : constant := 60;   --  Y of an "middle leg" (with X=0)

   Origin : constant array (1 .. 5) of Hexapod.Position :=
     (1 => (-X, -Y, Z),
      2 => (-X, Y, Z),
      3 => (0, M, Z),
      4 => (X, Y, Z),
      5 => (X, -Y, Z));  --  6 => (0, -M, Z)

   function DX (Index : Positive) return Distance;

   --------
   -- DX --
   --------

   function DX (Index : Positive) return Distance is
   begin
      if Index in 2 .. 4 then
         return 40;
      else
         return 60;
      end if;
   end DX;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : in out Unit'Class;
      Transport : GPIO.I2C.I2C_BSC1)
   is
      use type Hexapod.Legs.Angle;
      subtype Angle is Hexapod.Legs.Angle;

      package Elementary_Functions is
        new Ada.Numerics.Generic_Elementary_Functions (Hexapod.Legs.Angle);

      use Elementary_Functions;

      π : constant := Ada.Numerics.Pi;
      α : constant Angle := Arctan (Angle'Base (Y), Angle'Base (X));

      --        5  6  1
      --  X <------------
      --        4  3  2
      --           |
      --           v Y

      Rotated : constant array (Self.Legs'Range) of Hexapod.Legs.Angle :=
        (1 => π + α,
         2 => π - α,
         3 => π / 2.0,
         4 => α,
         5 => -α);  --  6 => 3.0 / 4.0 * π ?

      Address : constant W.Address := (others => False);
      Driver  : constant W.Servo_Driver_HAT := W.Create (Transport, Address);

   begin
      Self.Transport := Transport;

      for J in Self.Motors'Range loop
         Self.Motors (J) := Motors.Servo.Angle.MG995.Create
           (Driver, Shield.Motor_Drivers.Motor_Chanel_Number (J));
      end loop;

      for J in Self.Legs'Range loop
         Self.Legs (J).Configure
           (Segments  => (30, 85, 120),
            Origin    => Origin (J),
            Rotated   => Rotated (J),
            Motors    => Self.Motors (J * 3 - 2 .. J * 3),
            Scheduler => Self.Scheduler'Unchecked_Access);

      end loop;

      --  Move the leg to initial position
      for J in Self.Legs'Range loop

         Self.Legs (J).Assign_Program
           (Program =>
              (1 =>
                   (Tick   => 1.0,
                    Target => (Origin (J).X + DX (J),
                               Origin (J).Y + 69,
                               Origin (J).Z - 50),
                    Linear => False)),
            Repeat  => 1);

      end loop;
   end Initialize;

   ---------
   -- Run --
   ---------

   not overriding procedure Run (Self : in out Unit) is
   begin
      for J in Self.Legs'Range loop
         Self.Legs (J).Assign_Program
           (Program => Hexapod.Programs.Create_Step
              (From   => (Origin (J).X + DX (J),
                          Origin (J).Y + 69,
                          Origin (J).Z - 50),
              To      => (Origin (J).X - DX (J),
                          Origin (J).Y + 69,
                          Origin (J).Z - 50),
               Height => 20,
               Up     => 1.0,
               Down   => 4.0),
            Repeat  => 2);
      end loop;
   end Run;

end Hexapod.Units;
