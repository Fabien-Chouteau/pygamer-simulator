with HAL; use HAL;

with SDL;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Pixel_Formats;
with SDL.Video.Palettes;      use SDL.Video.Palettes;
with SDL.Video.Pixel_Formats; use SDL.Video.Pixel_Formats;
with SDL.Video.Textures;      use SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Images;
with SDL.Images.IO;

use SDL.Video;
with Interfaces.C; use Interfaces.C;
with SDL.Video.Pixels;
with Ada.Unchecked_Conversion;
with System;

package body PyGamer.Screen is

   Scroll_Val : UInt8 := 0;

   Cnt : UInt16 := 0;

   Pixel_Scale : constant := 3;

   W          : SDL.Video.Windows.Window;
   Renderer   : SDL.Video.Renderers.Renderer;
   Texture    : SDL.Video.Textures.Texture;
   SDL_Pixels : System.Address;

   XS, XE, YS, YE : Natural := 0;
   X, Y : Natural := 0;

   Debug_Enabled : Boolean := False;
   Debug_Color   : UInt16 := 100;

   type Texture_2D_Array is array (Natural range <>,
                                   Natural range <>)
     of aliased HAL.UInt16;

   type Texture_1D_Array is array (Natural range <>)
     of aliased HAL.UInt16;

   package Texture_2D is new SDL.Video.Pixels.Texture_Data
     (Index              => Natural,
      Element            => HAL.UInt16,
      Element_Array_1D   => Texture_1D_Array,
      Element_Array_2D   => Texture_2D_Array,
      Default_Terminator => 0);

   procedure Lock is new SDL.Video.Textures.Lock
     (Pixel_Pointer_Type => System.Address);

   function To_Address is
     new Ada.Unchecked_Conversion
       (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
        Target => System.Address);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         raise Program_Error with "SDL Video init failed";
      end if;

      SDL.Video.Windows.Makers.Create
        (W, "PyGamer Simulator",
         0,
         0,
         779,
         439,
         Flags    => SDL.Video.Windows.Windowed);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create
        (Tex      => Texture,
         Renderer => Renderer,
         Format   => SDL.Video.Pixel_Formats.Pixel_Format_RGB_565,
         Kind     => SDL.Video.Textures.Streaming,
         Size     => (Width, Height));

      if not SDL.Images.Initialise then
         raise Program_Error with "SDL Image init failed";
      end if;

   end Initialize;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address (X_Start, X_End, Y_Start, Y_End : HAL.UInt16) is
   begin
      XS := Integer (X_Start);
      YS := Integer (Y_Start);
      XE := Integer (X_End);
      YE := Integer (Y_End);
      X := XS;
      Y := YS;
      if XS < 0 then
         raise Program_Error;
      end if;
      if YS < 0 then
         raise Program_Error;
      end if;
      if XE >= Width then
         raise Program_Error;
      end if;
      if YE >= Height then
         raise Program_Error;
      end if;
      Cnt := Cnt + 1;
   end Set_Address;

   --------------------
   -- Start_Pixel_TX --
   --------------------

   procedure Start_Pixel_TX is
   begin
      Lock (Texture, SDL_Pixels);
   end Start_Pixel_TX;

   ------------------
   -- End_Pixel_TX --
   ------------------

   procedure End_Pixel_TX is
   begin
      Texture.Unlock;

      Renderer.Clear;

      --  Copy in two sections to simulate the ST7735R hardware scrolling

      Renderer.Copy (Texture,
                     From  => (0,
                               0,
                               int (Width) - int (Scroll_Val),
                               int (Height)),
                     To => (int (Scroll_Val) * Pixel_Scale,
                            0,
                            (int (Width) - int (Scroll_Val)) * Pixel_Scale,
                            int (Height) * Pixel_Scale));

      Renderer.Copy (Texture,
                     From  => (int (Width) - int (Scroll_Val),
                               0,
                               int (Scroll_Val),
                               int (Height)),
                     To => (0,
                            0,
                            int (Scroll_Val) * Pixel_Scale,
                            int (Height) * Pixel_Scale));
      Renderer.Present;
   end End_Pixel_TX;

   -----------------
   -- Push_Pixels --
   -----------------

   procedure Push_Pixels (Data : HAL.UInt16_Array) is
      Actual_Pixels : Texture_1D_Array (0 .. Natural (Width * Height - 1))
        with
          Address => SDL_Pixels;

      Swapped_Data : HAL.UInt16_Array (Data'Range);
   begin

      --  Byte swap is inverted in the PyGamer simulator
      for Index in Data'Range loop
         Swapped_Data (Index) := Shift_Right (Data (Index) and 16#FF00#, 8) or
           (Shift_Left (Data (Index), 8) and 16#FF00#);
      end loop;

      for Pix of Swapped_Data loop
         if Debug_Enabled then
            Actual_Pixels (X + Y * Width) := Debug_Color;
         else
            Actual_Pixels (X + Y * Width) := Pix;
         end if;

         if X = XE then
            X := XS;
            if Y = YE then
               Y := YS;
            else
               Y := Y + 1;
            end if;
         else
            X := X + 1;
         end if;
      end loop;

      if Debug_Enabled then
         Debug_Color := Debug_Color + 1000;
      end if;
   end Push_Pixels;

   -----------------
   -- Push_Pixels --
   -----------------

   procedure Push_Pixels_Swap (Data : in out HAL.UInt16_Array) is
      Actual_Pixels : Texture_1D_Array (0 .. Natural (Width * Height - 1))
        with
          Address => SDL_Pixels;

   begin
      for Pix of Data loop
         if Debug_Enabled then
            Actual_Pixels (X + Y * Width) := Debug_Color;
         else
            Actual_Pixels (X + Y * Width) := Pix;
         end if;

         if X = XE then
            X := XS;
            if Y = YE then
               Y := YS;
            else
               Y := Y + 1;
            end if;
         else
            X := X + 1;
         end if;
      end loop;

      if Debug_Enabled then
         Debug_Color := Debug_Color + 1000;
      end if;
   end Push_Pixels_Swap;

   ------------
   -- Scroll --
   ------------

   procedure Scroll (Val : HAL.UInt8) is
   begin
      Scroll_Val := Val mod Width;
   end Scroll;

   procedure Debug_Mode (Enabled : Boolean) is
   begin
      Debug_Enabled := Enabled;
   end Debug_Mode;

begin
   Initialize;
end PyGamer.Screen;
