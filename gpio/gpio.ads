
---------------------------------------
--  Raspberry Pi3 ( bcm2837B0 ) GPIO --
---------------------------------------

private with Ada.Finalization;
private with System.Storage_Elements;

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

   function Is_High  (Self : Input_Pin) return Boolean;
   function Is_Low   (Self : Input_Pin) return Boolean;
   function Wait_Low (Self : Input_Pin; Timaout : Duration) return Boolean;

   type GPIO_Pin_Pull_Up_Down is (Off, Pull_Down, Pull_Up);
   for GPIO_Pin_Pull_Up_Down use (Off => 0, Pull_Down => 1, Pull_Up => 2);
   for GPIO_Pin_Pull_Up_Down'Size use 2;

   procedure Set_Pull_Up_Down
     (Self : Input_Pin;
      Mode : GPIO_Pin_Pull_Up_Down);

   Empty_Input_Pin : constant Input_Pin;

   function Create
     (Number : GPIO_Number)
      return Input_Pin;
   --  Initialize and return pin. May raise Program_Error
   --  if corresponding pin is already in use.

private
   use System, System.Storage_Elements;

   --  GPIO Mode type (FSEL) --

   type GPIO_FSEL_Mode is (Input, Output, Alt_0);
   for GPIO_FSEL_Mode use (Input => 0, Output => 1, Alt_0 => 2#100#);
   for GPIO_FSEL_Mode'Size use 3;

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

   -- Input_Pin --

   type Input_Pin is new Pin with null record;

   Empty_Input_Pin : constant Input_Pin :=
     Input_Pin'(Ada.Finalization.Controlled with Node => null);

   -- Output_Pin --

   type Output_Pin is new Pin with null record;

   overriding procedure Finalize (Self : in out Output_Pin);

   Empty_Output_Pin : constant Output_Pin :=
     Output_Pin'(Ada.Finalization.Controlled with Node => null);

   -- Alternate_Pin --

   type Alternate_Pin is new Pin with null record;

   function Create
     (Number : GPIO_Number;
      Mode   : GPIO_FSEL_Mode)
      return Alternate_Pin;
   --  Initialize and return alternative pin. May raise Program_Error
   --  if corresponding pin is already in use.

   Empty_Alternate_Pin : constant Alternate_Pin :=
     Alternate_Pin'(Ada.Finalization.Controlled with Node => null);

   type Bit is range 0 .. 1;
   for Bit'Size use 1;

end GPIO;
