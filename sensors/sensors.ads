
with Ada.Finalization;
with GPIO;

package Sensors is

   type Sensor is abstract tagged private;

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
