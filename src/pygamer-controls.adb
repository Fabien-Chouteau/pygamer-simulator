with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;

package body PyGamer.Controls is

   type Buttons_State is array (Buttons) of Boolean;

   Current_Pressed : Buttons_State := (others => False);
   Previous_Pressed : Buttons_State := (others => False);

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin

      Previous_Pressed := Current_Pressed;

      Current_Pressed (A) := SFML_Pressed (A);
      Current_Pressed (B) := SFML_Pressed (B);
      Current_Pressed (Up) := SFML_Pressed (Up);
      Current_Pressed (Down) := SFML_Pressed (Down);
      Current_Pressed (Left) := SFML_Pressed (Left);
      Current_Pressed (Right) := SFML_Pressed (Right);
      Current_Pressed (Sel) := SFML_Pressed (Sel);
      Current_Pressed (Start) := SFML_Pressed (Start);
   end Scan;

   -------------
   -- Pressed --
   -------------

   function Pressed (Button : Buttons) return Boolean
   is (Current_Pressed (Button));

   ------------
   -- Rising --
   ------------

   function Rising  (Button : Buttons) return Boolean
   is (Previous_Pressed (Button) and then not Current_Pressed (Button));

   -------------
   -- Falling --
   -------------

   function Falling (Button : Buttons) return Boolean
   is (not Previous_Pressed (Button) and then Current_Pressed (Button));

   ----------------
   -- Joystick_X --
   ----------------

   function Joystick_X return Joystick_Range
   is (if Pressed (Left) then Joystick_Range'First
       elsif Pressed (Right) then Joystick_Range'Last
       else 0);

   ----------------
   -- Joystick_Y --
   ----------------

   function Joystick_Y return Joystick_Range
   is (if Pressed (Up) then Joystick_Range'First
       elsif Pressed (Down) then Joystick_Range'Last
       else 0);

end PyGamer.Controls;
