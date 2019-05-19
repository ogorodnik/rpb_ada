with Ada.Numerics.Generic_Elementary_Functions;

package body Hexapod.Legs is

   type Real is digits Angle'Digits;

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real);

   --------------------
   -- Compute_Angles --
   --------------------

   not overriding procedure Compute_Angles
     (Self     : Leg;
      Position : Hexapod.Legs.Position;
      Angles   : out Segment_Angles)
   is
      use Elementary_Functions;
      --  Two equations system:
      --  S3*cos(E) = H + S2*cos(D)
      --  S3*sin(E) = L + S2*sin(D)
      --  where C = E + D

      Y  : constant Real := Real (Position.Y) - Real (Self.Origin.Y);
      X  : constant Real := Real (Position.X) - Real (Self.Origin.X);
      A  : constant Real := Arctan (Y => X, X => Y);
      --  From now work with projection to (Z, Y) plane. First scale S1..3, L
      S1 : constant Real := Real (Self.Segments.S1) * Cos (A);
      S2 : constant Real := Real (Self.Segments.S2) * Cos (A);
      S3 : constant Real := Real (Self.Segments.S3) * Cos (A);
      L  : constant Real := Y * Cos (A) - S1;
      H  : constant Real := -(Real (Position.Z) - Real (Self.Origin.Z));
      V1 : constant Real := S3 ** 2 - H ** 2 - L ** 2 - S2 ** 2;
      V2 : constant Real := 2.0 * L * S2;
      V3 : constant Real := 2.0 * H * S2;
      V4 : constant Real := Sqrt (V2 ** 2 + V3 ** 2);
      D  : constant Real := Arcsin (V1 / V4) - Arccos (V2 / V4);
      E  : constant Real := Arcsin ((L + S2 * Sin (D)) / S3);
   begin
      Angles :=
        (S1 => Angle (A) - Self.Rotated,
         S2 => Angle (π / 2.0 - D),
         S3 => Angle (E - D));
   end Compute_Angles;

   ---------------
   -- Configure --
   ---------------

   not overriding procedure Configure
     (Self     : in out Leg;
      Segments : Hexapod.Legs.Segments;
      Origin   : Position;
      Rotated  : Angle) is
   begin
      Self.Segments := Segments;
      Self.Origin := Origin;
      Self.Rotated := Rotated;
   end Configure;

end Hexapod.Legs;
