
with Ada.Unchecked_Deallocation;

package body Shield is

   procedure Free is new Ada.Unchecked_Deallocation
     (Root_Shield_Node'Class, Root_Shield_Node_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Root_Shield) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Root_Shield) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter - 1;
         if Self.Node.Counter = 0 then
            Free (Self.Node);
         end if;
      end if;
   end Finalize;

end Shield;
