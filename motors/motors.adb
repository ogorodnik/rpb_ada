
with Ada.Unchecked_Deallocation;

package body Motors is

   procedure Free is
     new Ada.Unchecked_Deallocation (Motor_Node'Class, Motor_Node_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Shield.Motor_Drivers.Motor_Driver'Class, Motor_Driver_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Motor) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Motor) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter - 1;
         if Self.Node.Counter = 0 then
            Free (Self.Node.Driver);
            Free (Self.Node);
         end if;
      end if;
   end Finalize;

end Motors;
