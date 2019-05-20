with Ada.Calendar;
private with Ada.Containers.Ordered_Maps;

package Hexapod.Schedulers is

   type Scheduler is tagged limited private;
   --  Scheduler monitors listeners and calls their callbacks once time over.

   type Scheduler_Access is access all Scheduler'Class;
   for Scheduler_Access'Storage_Size use 0;

   type Listener is limited interface;

   type Listener_Access is access all Listener'Class;
   for Listener_Access'Storage_Size use 0;

   not overriding procedure Callback
     (Self : in out Listener;
      Time : Ada.Calendar.Time) is abstract;
   --  This is called (only once) when time is over

   not overriding procedure Add_Listener
     (Self : in out Scheduler;
      Item : Listener_Access;
      Time : Ada.Calendar.Time);
   --  Call Item.Callback at given Time

   not overriding procedure Remove_Listener
     (Self : in out Scheduler;
      Item : Listener_Access);
   --  Drop Item from execution queue

   not overriding procedure Call_Callbacks
     (Self : in out Scheduler;
      Time : in out Ada.Calendar.Time);
   --  Trigger listeners at given Time. Return next Time to run this again

private

   type Unique_Identifier is mod 2 ** 32;

   type Key_In_Map is record
      Time   : Ada.Calendar.Time;
      Unique : Unique_Identifier;
   end record;

   function Less (Left, Right : Key_In_Map) return Boolean;

   package Listeners_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Key_In_Map,
      Element_Type => Listener_Access,
      "<"          => Less);

   type Scheduler is tagged limited record
      Map  : Listeners_Maps.Map;
      Last : Unique_Identifier := 0;
   end record;

end Hexapod.Schedulers;
