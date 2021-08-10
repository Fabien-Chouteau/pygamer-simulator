with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Sf.Window.VideoMode; use Sf.Window.VideoMode;
with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.Sprite; use Sf.Graphics.Sprite;
with Sf.Graphics.Texture; use Sf.Graphics.Texture;
with Sf.Graphics.RenderTexture; use Sf.Graphics.RenderTexture;
with Sf.Graphics.View; use Sf.Graphics.View;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
with Sf.Window.Window; use Sf.Window.Window;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Keyboard; use Sf.Window.Keyboard;
with Sf; use Sf;

with Ada.Real_Time; use Ada.Real_Time;
with Sf.System.Vector2; use Sf.System.Vector2;

with Ada.Unchecked_Conversion;
with simulator_assets;

package body PyGamer is

   function Hack_SF_Binding is new Ada.Unchecked_Conversion (sfSprite_Ptr,
                                                             sfView_Ptr);

   --------------
   -- Set_View --
   --------------

   procedure Set_View (View : Sf.Graphics.sfView_Ptr;
                       Width, Height : sfUint32)
   is
      Win_Ratio  : constant Float := Float (Width) / Float (Height);
      View_Ratio : constant Float := Float (getSize (View).x) / Float (getSize (View).y);
      Size_X : Float := 1.0;
      Size_Y : Float := 1.0;
      Pos_X : Float := 0.0;
      Pos_Y : Float := 0.0;

      Horizontal_Spacing : Boolean := True;
   begin
      if Win_Ratio < View_Ratio then
         Size_Y := Win_Ratio / View_Ratio;
         Pos_Y := (1.0 - Size_Y) / 2.0;
      else
         Size_X := View_Ratio / Win_Ratio;
         Pos_X := (1.0 - Size_X) / 2.0;
      end if;

      setViewport (View, (Pos_X, Pos_Y, Size_X, Size_Y));
   end Set_View;

   task Periodic_Update is

   end Periodic_Update;

   ---------------------
   -- Periodic_Update --
   ---------------------

   task body Periodic_Update is
      BG_Width : constant := 730;
      BG_Height : constant := 411;
      Mode   : constant Sf.Window.VideoMode.sfVideoMode :=
        (BG_Width, BG_Height, 32);

      Params : sfContextSettings := sfDefaultContextSettings;
      Window : Sf.Graphics.sfRenderWindow_Ptr;
      Framebuffer_Texture : Sf.Graphics.sfTexture_Ptr;
      Render_Texture : Sf.Graphics.sfRenderTexture_Ptr;
      PG_Texture : Sf.Graphics.sfTexture_Ptr;
      PG_Sprite : Sf.Graphics.sfSprite_Ptr;
      Screen_Sprite : Sf.Graphics.sfSprite_Ptr;
      Sprite_Left : Sf.Graphics.sfSprite_Ptr;
      Sprite_Right : Sf.Graphics.sfSprite_Ptr;
      Letter_Box_View : Sf.Graphics.sfView_Ptr;
      Event   : sfEvent;

      Period : constant Time_Span := Milliseconds (1000 / 60);
      Next_Release : Time := Clock + Period;
      Pressed : Boolean;

      Screen_Scale : constant := 287.0 / Float (Screen_Width);
      Screen_Offset : constant sfVector2f := (231.0, 66.0);
   begin

      Framebuffer_Texture := Create (Screen_Width, Screen_Height);
      if Framebuffer_Texture = null then
         Put_Line ("Failed to create screen texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         Ptr : constant simulator_assets.Content_Access :=
           simulator_assets.Get_Content ("pygamer.png").Content;
      begin
         PG_Texture := createFromMemory (Ptr.all'Address, Ptr.all'Length);
      end;

      if PG_Texture = null then
         Put_Line ("Failed to create PyGamer texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Render_Texture := create (Screen_Width, Screen_Height, False);
      if Render_Texture = null then
         Put_Line ("Could not create render texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Screen_Sprite := Create;
      if Screen_Sprite = null then
         Put_Line ("Could not create screen sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      SetTexture (Screen_Sprite, getTexture (Render_Texture));
      scale (Screen_Sprite, (Screen_Scale, Screen_Scale));
      setPosition (Screen_Sprite, Screen_Offset);

      Sprite_Left := Create;
      if Sprite_Left = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      SetTexture (Sprite_Left, Framebuffer_Texture);

      Sprite_Right := Create;
      if Sprite_Right = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      SetTexture (Sprite_Right, Framebuffer_Texture);

      PG_Sprite := Create;
      if PG_Sprite = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      SetTexture (PG_Sprite, PG_Texture);

      Window := Create (Mode, "PyGamer simulator", sfResize or sfClose, Params);
      if Window = null then
         Put_Line ("Failed to create window");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      SetVerticalSyncEnabled (Window, sfFalse);
      SetVisible (Window, sfTrue);

      Letter_Box_View := create;
      if Letter_Box_View = null then
         Put_Line ("Failed to create view");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setSize (Letter_Box_View, (Float (BG_Width), Float (BG_Height)));
      setCenter (Letter_Box_View,
                 (Float (BG_Width) / 2.0, Float (BG_Height) / 2.0));

      Set_View (Letter_Box_View, getSize (Window).x, getSize (Window).y);

      loop
         delay until Next_Release;
         Next_Release := Next_Release + Period;

         while pollEvent (Window, Event) loop

            if Event.eventType = sfEvtClosed then
               Close (Window);
               Put_Line ("Attempting to close");
               GNAT.OS_Lib.OS_Exit (0);
            end if;

            if Event.eventType = sfEvtResized then
               Set_View (Letter_Box_View, Event.size.width, Event.size.height);
            end if;

            if Event.eventType in sfEvtKeyPressed | sfEvtKeyReleased then

               Pressed := Event.eventType = sfEvtKeyPressed;

               case Event.key.code is
               when sfKeyLeft =>
                  SFML_Pressed (Left) := Pressed;
               when sfKeyRight =>
                  SFML_Pressed (Right) := Pressed;
               when sfKeyDown =>
                  SFML_Pressed (Down) := Pressed;
               when sfKeyUp =>
                  SFML_Pressed (Up) := Pressed;
               when sfKeyZ =>
                  SFML_Pressed (B) := Pressed;
               when sfKeyX =>
                  SFML_Pressed (A) := Pressed;
               when sfKeyReturn =>
                  SFML_Pressed (Start) := Pressed;
               when sfKeyC =>
                  SFML_Pressed (Sel) := Pressed;
               when sfKeyEscape =>
                  Close (Window);
                  Put_Line ("Attempting to close");
                  GNAT.OS_Lib.OS_Exit (0);
               when others =>
                  null;
               end case;
            end if;
         end loop;

         updateFromPixels (texture => Framebuffer_Texture,
                           pixels  => Frame_Buffer (Frame_Buffer'First)'access,
                           width   => Screen_Width,
                           height  => Screen_Height,
                           x       => 0,
                           y       => 0);

         setPosition (Sprite_Right,
                      (Float (Scroll_Val) - Float (Screen_Width), 0.0));
         setPosition (Sprite_Left,
                      (Float (Scroll_Val), 0.0));

         drawSprite (Render_Texture, Hack_SF_Binding (Sprite_Right));
         drawSprite (Render_Texture, Hack_SF_Binding (Sprite_Left));
         display (Render_Texture);

         clear (Window);
         drawSprite (Window, PG_Sprite);
         drawSprite (Window, Screen_Sprite);

         setView (Window, Letter_Box_View);
         display (Window);
      end loop;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (1);
   end Periodic_Update;

end PyGamer;

