
----------------------------------------------------------------------
--  Base package for all sensors which can be attached to Raspberry --
----------------------------------------------------------------------

with Ada.Finalization;

package Sensors is

   type Sensor is abstract new Ada.Finalization.Controlled with private;

private

   type Sensor_Node is tagged record
      Counter : Natural := 0;
   end record;
   type Sensor_Node_Access is access all Sensor_Node'Class;

   type Sensor is new Ada.Finalization.Controlled with record
      Node : Sensor_Node_Access;
   end record;

   overriding procedure Adjust   (Self : in out Sensor);
   overriding procedure Finalize (Self : in out Sensor);

end Sensors;
