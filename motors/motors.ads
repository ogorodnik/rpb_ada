
----------------------------------
--  Base package for all motors --
----------------------------------

private with Ada.Finalization;
private with Shield.Motor_Drivers;

package Motors is

   type Motor is abstract tagged private;

private

   type Motor_Driver_Access is
     access all Shield.Motor_Drivers.Motor_Driver'Class;

   type Motor_Node is tagged record
      Counter : Natural := 0;
      Driver  : Motor_Driver_Access;
   end record;
   type Motor_Node_Access is access all Motor_Node'Class;

   type Motor is new Ada.Finalization.Controlled with record
      Node : Motor_Node_Access;
   end record;

   overriding procedure Adjust   (Self : in out Motor);
   overriding procedure Finalize (Self : in out Motor);

end Motors;
