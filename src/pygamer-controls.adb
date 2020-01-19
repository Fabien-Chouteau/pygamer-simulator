with SDL.Events;           use SDL.Events;
with SDL.Events.Events;    use SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;

with GNAT.OS_Lib;

package body PyGamer.Controls is

   type Buttons_State is array (Buttons) of Boolean;

   Current_Pressed : Buttons_State := (others => False);
   Previous_Pressed : Buttons_State := (others => False);

   ----------
   -- Scan --
   ----------

   procedure Scan is
      Event   : SDL.Events.Events.Events;
      Pressed : Boolean;
   begin
      Previous_Pressed := Current_Pressed;

      while SDL.Events.Events.Poll (Event) loop

         if Event.Common.Event_Type in Key_Down | Key_Up then

            Pressed := Event.Common.Event_Type = Key_Down;

            case Event.Keyboard.Key_Sym.Scan_Code is
            when Scan_Code_Left =>
               Current_Pressed (Left) := Pressed;
            when Scan_Code_Right =>
               Current_Pressed (Right) := Pressed;
            when Scan_Code_Down =>
               Current_Pressed (Down) := Pressed;
            when Scan_Code_Up =>
               Current_Pressed (Up) := Pressed;
            when Scan_Code_Z =>
               Current_Pressed (B) := Pressed;
            when Scan_Code_X =>
               Current_Pressed (A) := Pressed;
            when Scan_Code_Return =>
               Current_Pressed (Start) := Pressed;
            when Scan_Code_C =>
               Current_Pressed (Sel) := Pressed;
            when Scan_Code_Escape =>
               GNAT.OS_Lib.OS_Exit (0);
            when others =>
               null;
            end case;
         end if;
      end loop;
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
