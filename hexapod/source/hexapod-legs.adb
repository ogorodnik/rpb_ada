with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body Hexapod.Legs is

   type Real is digits Angle'Digits;

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real);

   --------------------
   -- Assign_Program --
   --------------------

   not overriding procedure Assign_Program
     (Self    : in out Leg;
      Program : Hexapod.Programs.Program_Item_Array;
      Repeat  : Positive) is
   begin
      Self.Scheduler.Remove_Listener (Self'Unchecked_Access);

      Self.Program.Clear;

      for J in Program'Range loop
         Self.Program.Append (Program (J));
      end loop;

      Self.Item_Index := 0;
      Self.Repeat := Repeat;

      Self.Next_Program_Item;  --  Start executing first item
   end Assign_Program;

   -----------------------
   -- Next_Program_Item --
   -----------------------

   not overriding procedure Next_Program_Item (Self : aliased in out Leg) is
      use type Ada.Calendar.Time;
   begin
      if Self.Item_Index = Self.Program.Last_Index then
         Self.Item_Index := 0;
         if Self.Repeat > 1 then
            Self.Repeat := Self.Repeat - 1;

            if Self.Repeat = 0 then
               return;
            end if;
         else
            return;
         end if;
      end if;

      Self.Item_Index := Self.Item_Index + 1;
      Self.Current_Item.Started := Ada.Calendar.Clock;
      Self.Current_Item.Origin := Self.Current_Item.Item.Target;
      Self.Current_Item.Item := Self.Program (Self.Item_Index);
      Self.Current_Item.Finished := Self.Current_Item.Started
        + Self.Current_Item.Item.Tick;

      if Self.Current_Item.Item.Linear then
         Self.Scheduler.Add_Listener
           (Self'Unchecked_Access,
            Self.Current_Item.Started + 0.020);  --  One frame duration
      else
         Self.Rotate_Joints (Self.Current_Item.Item.Target);
         Self.Scheduler.Add_Listener
           (Self'Unchecked_Access,
            Self.Current_Item.Finished);
      end if;
   end Next_Program_Item;

   --------------
   -- Callback --
   --------------

   overriding procedure Callback
     (Self : in out Leg;
      Time : Ada.Calendar.Time)
   is
      use type Ada.Calendar.Time;

      function Interpolate
        (Origin, Target : Distance;
         Passed, Total  : Duration) return Distance;

      function Interpolate
        (Origin, Target : Distance;
         Passed, Total  : Duration) return Distance
      is
         Step : constant Float :=
           Float (Target - Origin) * Float (Passed) / Float (Total);
      begin
         return Origin + Distance (Step);
      end Interpolate;

      Tick   : constant Duration := Self.Current_Item.Item.Tick;
      Passed : constant Duration := Time - Self.Current_Item.Started;
      Origin : constant Position := Self.Current_Item.Origin;
      Target : constant Position := Self.Current_Item.Item.Target;
      Next   : Position;
   begin
      if Passed >= Tick then
         Self.Next_Program_Item;
      else
         Next :=
           (X => Interpolate (Origin.X, Target.X, Passed, Tick),
            Y => Interpolate (Origin.Y, Target.Y, Passed, Tick),
            Z => Interpolate (Origin.Z, Target.Z, Passed, Tick));

         Self.Rotate_Joints (Next);

         Self.Scheduler.Add_Listener
           (Self'Unchecked_Access, Time + 0.020);
      end if;
   end Callback;

   --------------------
   -- Compute_Angles --
   --------------------

   not overriding procedure Compute_Angles
     (Self     : Leg;
      Position : Hexapod.Position;
      Angles   : out Joint_Angles)
   is
      use Elementary_Functions;
      --  Two equations system:
      --  S3*cos(E) = H + S2*cos(D)
      --  S3*sin(E) = L + S2*sin(D)
      --  where C = E + (-D) in [0 .. π]
      --  On the picture D is clockwise, so negative

      Y  : constant Real := Real (Position.Y) - Real (Self.Origin.Y);
      X  : constant Real := Real (Position.X) - Real (Self.Origin.X);
      A  : constant Real range -π .. π := Arctan (Y => Y, X => X);
      --  From now work with projection to (Z, Y) plane. First scale S1..S3, L
      S1 : constant Real := Real (Self.Segments.S1) * abs Sin (A);
      S2 : constant Real := Real (Self.Segments.S2) * abs Sin (A);
      S3 : constant Real := Real (Self.Segments.S3) * abs Sin (A);
      L  : constant Real := abs (Y * Sin (A)) - S1;
      H  : constant Real := -(Real (Position.Z) - Real (Self.Origin.Z));
      V1 : constant Real := S3 ** 2 - H ** 2 - L ** 2 - S2 ** 2;
      V2 : constant Real := 2.0 * L * S2;  --  > 0
      V3 : constant Real := 2.0 * H * S2;
      V4 : constant Real := Sqrt (V2 ** 2 + V3 ** 2);  -- > 0

      --------------------  [-π/2 .. π/2]    - [0 .. π/2]
      D  : constant Real := Arcsin (V1 / V4) - Arccos (V2 / V4);
      --------------------  [-π .. π/2]

      --------------------  [-π/2 .. π/2]
      E  : constant Real := Arcsin ((L + S2 * Sin (D)) / S3);
   begin
      Angles :=
        (S1 => Angle (A) - Self.Rotated,
         S2 => Angle (-D),
         S3 => Angle (E - D));
   end Compute_Angles;

   ---------------
   -- Configure --
   ---------------

   not overriding procedure Configure
     (Self      : in out Leg;
      Segments  : Hexapod.Legs.Segments;
      Origin    : Position;
      Rotated   : Angle;
      Motors    : Motor_Array;
      Scheduler : not null Hexapod.Schedulers.Scheduler_Access) is
   begin
      Self.Segments := Segments;
      Self.Origin := Origin;
      Self.Rotated := Rotated;
      Self.Joints :=
        (Motors (Motors'First),
         Motors (Motors'First + 1),
         Motors (Motors'First + 2));
      Self.Scheduler := Scheduler;
   end Configure;

   -------------------
   -- Rotate_Joints --
   -------------------

   not overriding procedure Rotate_Joints
     (Self   : in out Leg;
      Target : Position)
   is
      procedure Rotate (Index : Positive; Value : Angle);

      ------------
      -- Rotate --
      ------------

      procedure Rotate (Index : Positive; Value : Angle) is
         use type Motors.Servo.Angle.Angle_Type;

         Angle : constant Motors.Servo.Angle.Angle_Type :=
           Motors.Servo.Angle.Angle_Type (Value * 180.0 / π);
      begin
         if Self.Joint_Angles (Index) /= Angle then
            Self.Joints (Index).Set_Angle (Angle);
            Self.Joint_Angles (Index) := Angle;
         end if;

         Ada.Text_IO.Put (Angle'Img);
      end Rotate;

      Angles : Joint_Angles;

   begin
      Self.Compute_Angles (Target, Angles);
      Rotate (1, Angles.S1);
      Rotate (2, Angles.S2);
      Rotate (3, Angles.S3);

      Ada.Text_IO.New_Line;
   end Rotate_Joints;

end Hexapod.Legs;
