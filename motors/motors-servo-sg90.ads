
-----------------
--  SG90 motor --
-----------------

with Shield.Motor_Drivers.Servo; use Shield.Motor_Drivers.Servo;

package Motors.Servo.SG90 is

   type SG90_Motor is new Servo_Motor with private;

   function Create
     (Driver : Servo_Motor_Driver'Class;
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number)
      return SG90_Motor;

   overriding procedure Set_Angle
     (Self  : SG90_Motor;
      Angle : Angle_Type);

   function Get_Min (Self : SG90_Motor) return PWM_Value;
   function Get_Max (Self : SG90_Motor) return PWM_Value;

   procedure Set_Min (Self : SG90_Motor; Value : PWM_Value);
   procedure Set_Max (Self : SG90_Motor; Value : PWM_Value);

private

   type Servo_Motor_Driver_Access is access all Servo_Motor_Driver'Class;

   type SG90_Motor_Node is new Motor_Node with record
      Chanel : Shield.Motor_Drivers.Motor_Chanel_Number;
      Min    : PWM_Value;
      Max    : PWM_Value;
   end record;
   type SG90_Motor_Node_Access is access all SG90_Motor_Node;

   type SG90_Motor is new Servo_Motor with null record;

end Motors.Servo.SG90;
