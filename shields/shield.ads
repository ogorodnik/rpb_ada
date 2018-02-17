
private with Ada.Finalization;

package Shield is

   type Root_Shield is abstract tagged private;

private

   type Root_Shield_Node is tagged record
      Counter  : Natural := 0;
   end record;
   type Root_Shield_Node_Access is access all Root_Shield_Node'Class;

   type Root_Shield is new Ada.Finalization.Controlled with record
      Node : Root_Shield_Node_Access;
   end record;

   overriding procedure Adjust   (Self : in out Root_Shield);
   overriding procedure Finalize (Self : in out Root_Shield);

end Shield;
