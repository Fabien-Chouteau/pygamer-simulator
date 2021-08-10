private with sf.Graphics;
private with sf.Graphics.Image;
private with Sf;
private with HAL;

package PyGamer is
   pragma Elaborate_Body;

private

   Screen_Width : constant := 160;
   Screen_Height : constant := 128;

   Frame_Buffer : array (0 .. (Screen_Width * Screen_Height * 4) - 1) of
     aliased Sf.sfUInt8
     := (others => 127);

   Scroll_Val : HAL.UInt8 := 0;

   type SFML_Keys is (A, B, Left, Right, Up, Down, Sel, Start);
   SFML_Pressed : array (SFML_Keys) of Boolean := (others => False);
   pragma Volatile_Components (SFML_Pressed);

end PyGamer;
