with Hexapod.Legs;
private with Hexapod.Schedulers;

with GPIO.I2C;
with Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

package Hexapod.Units is

   type Unit is tagged limited private;
   --  This type represent hexapod itself.

   procedure Initialize
     (Self      : in out Unit'Class;
      Transport : GPIO.I2C.I2C_BSC1);
   --  Initialize internal data of the hexapod

   not overriding procedure Run (Self : in out Unit);
   --  Let hexapod go

private

   package W renames Shield.Motor_Drivers.Servo.WAVESHARE_Servo_Driver_HAT;

   type Leg_Array is array (Positive range <>) of Hexapod.Legs.Leg;

   type Unit is tagged limited record
      Transport : GPIO.I2C.I2C_BSC1;   --  Do we need this here?
      Driver    : W.Servo_Driver_HAT;  --  Do we need this here?
      Scheduler : aliased Hexapod.Schedulers.Scheduler;
      Motors    : Hexapod.Legs.Motor_Array (1 .. 5 * 3);  --  Do we need this?
      Legs      : Leg_Array (1 .. 5);
   end record;

end Hexapod.Units;
