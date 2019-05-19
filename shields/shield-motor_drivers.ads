
-----------------------------------------
--  Base package for all motor drivers --
-----------------------------------------

package Shield.Motor_Drivers is

   type Motor_Chanel_Number is new Natural;

   type Motor_Driver is abstract new Root_Shield with private;

private

   type Motor_Driver is abstract new Root_Shield with null record;

end Shield.Motor_Drivers;
