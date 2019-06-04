
----------------------------------------
--  Base package for all servo motors --
----------------------------------------

with Shield.Motor_Drivers.Servo; use Shield.Motor_Drivers.Servo;

package Motors.Servo is

   type Servo_Motor is abstract new Motor with private;

   function Get_Min (Self : Servo_Motor) return PWM_Value;
   function Get_Max (Self : Servo_Motor) return PWM_Value;

   procedure Set_Min (Self : Servo_Motor; Value : PWM_Value);
   procedure Set_Max (Self : Servo_Motor; Value : PWM_Value);

private

   type Servo_Motor_Driver_Access is access all Servo_Motor_Driver'Class;

   type Servo_Motor_Node is new Motor_Node with record
      Chanel  : Shield.Motor_Drivers.Motor_Chanel_Number;
      Min     : PWM_Value;
      Max     : PWM_Value;
      MHz_Min : Frequency;
      MHz_Max : Frequency;
   end record;
   type Servo_Motor_Node_Access is access all Servo_Motor_Node;

   type Servo_Motor is abstract new Motor with null record;

   procedure Check_Driver_Settings (Self : Servo_Motor'Class);
   procedure Do_Check_Driver_Settings (Self : Servo_Motor) is null;

end Motors.Servo;
