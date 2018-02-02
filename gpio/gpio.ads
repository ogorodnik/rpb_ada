
--  For Raspberry Pi3 --

private with Ada.Finalization;

package GPIO is

   type GPIO_Number is range 0 .. 27;

   type Pin is abstract tagged private;

   function Id (Self : Pin) return GPIO_Number;
   --  Return a number of a pin

   -- Output_Pin --

   type Output_Pin is new Pin with private;

   procedure High (Self : in out Output_Pin);
   procedure Low  (Self : in out Output_Pin);

   Empty_Output_Pin : constant Output_Pin;

   function Create
     (Number : GPIO_Number)
      return Output_Pin;
   --  Initialize and return pin. May raise Program_Error
   --  if corresponding pin is already in use.

   -- Input_Pin --

   type Input_Pin is new Pin with private;

   function Is_High (Self : Input_Pin) return Boolean;
   function Is_Low  (Self : Input_Pin) return Boolean;

   Empty_Input_Pin : constant Input_Pin;

   function Create
     (Number : GPIO_Number)
      return Input_Pin;
   --  Initialize and return pin. May raise Program_Error
   --  if corresponding pin is already in use.

private
   type Pin_Node (Number : GPIO_Number) is record
      Counter  : Natural     := 0;
      Is_High  : Boolean     := False;
      GPIO_Pin : GPIO_Number := 0;
   end record;
   type Pin_Node_Access is access all Pin_Node;

   type Pin is new Ada.Finalization.Controlled with record
      Node : Pin_Node_Access;
   end record;

   overriding procedure Adjust   (Self : in out Pin);
   overriding procedure Finalize (Self : in out Pin);

   type Output_Pin is new Pin with null record;

   overriding procedure Finalize (Self : in out Output_Pin);

   Empty_Output_Pin : constant Output_Pin :=
     Output_Pin'(Ada.Finalization.Controlled with Node => null);

   type Input_Pin is new Pin with null record;

   Empty_Input_Pin : constant Input_Pin :=
     Input_Pin'(Ada.Finalization.Controlled with Node => null);

end GPIO;
