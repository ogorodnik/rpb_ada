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
      Position : Leg_Position;
      Angles   : out Segment_Angles)
   is
      use Elementary_Functions;
      --  Two equations system:
      --  S3*cos(E) = H + S2*cos(D)
      --  S3*sin(E) = L - S2*sin(D)
      --  where C = E + D

      A : constant Real := Arctan
        (Y => Real (Position.X), X => Real (Position.Y));
      --  From now work with projection to (Z, Y) plane. First scale S1..3, L
      S1 : constant Real := Real (Self.Segments.S1) * Cos (A);
      S2 : constant Real := Real (Self.Segments.S2) * Cos (A);
      S3 : constant Real := Real (Self.Segments.S3) * Cos (A);
      L : constant Real := Real (Position.Y) * Cos (A) - S1;
      H : constant Real := Real (-Position.Z);
      V1 : constant Real := S3 ** 2 - H ** 2 - L ** 2 - S2 ** 2;
      V2 : constant Real := -2.0 * L * S2;
      V3 : constant Real := 2.0 * H * S2;
      V4 : constant Real := Sqrt (V2 ** 2 + V3 ** 2);
      D  : Real := Arcsin (V1 / V4) - Arccos (V2 / V4);
      E  : Real := Arcsin ((L - S2 * Sin (D)) / S3);
   begin
      if D < 0.0 then
         D := D + π;
      end if;

      E := Arcsin ((L - S2 * Sin (D)) / S3);
      Angles := (Angle (A), Angle (π / 2.0 + D), Angle (D + E));
   end Compute_Angles;

   ---------------
   -- Configure --
   ---------------

   not overriding procedure Configure
     (Self     : in out Leg;
      Segments : Hexapod.Legs.Segments) is
   begin
      Self.Segments := Segments;
   end Configure;

end Hexapod.Legs;
