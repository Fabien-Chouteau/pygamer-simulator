package PyGamer.Controls is

   procedure Scan;

   type Buttons is (A, B, Left, Right, Up, Down, Sel, Start);
   --  The Left, Right, Up, Down buttons are simulated from the position of the
   --  joystick.

   function Pressed (Button : Buttons) return Boolean;
   function Rising  (Button : Buttons) return Boolean;
   function Falling (Button : Buttons) return Boolean;

   type Joystick_Range is range -128 .. 127;

   function Joystick_X return Joystick_Range;
   function Joystick_Y return Joystick_Range;

end PyGamer.Controls;
