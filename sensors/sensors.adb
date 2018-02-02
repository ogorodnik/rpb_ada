
with Ada.Unchecked_Deallocation;

package body Sensors is

   procedure Free is
     new Ada.Unchecked_Deallocation (Sensor_Node'Class, Sensor_Node_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Sensor) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Sensor) is
   begin
      if Self.Node /= null then
         Self.Node.Counter := Self.Node.Counter - 1;
         if Self.Node.Counter = 0 then
            Free (Self.Node);
         end if;
      end if;
   end Finalize;

end Sensors;
