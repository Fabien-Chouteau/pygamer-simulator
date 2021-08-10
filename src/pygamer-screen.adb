with Ada.Text_IO; use Ada.Text_IO;

with HAL; use HAL;

with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;
with System;

with sf.Graphics.RenderWindow; use sf.Graphics.RenderWindow;
with Sf.Graphics.Color; use Sf.Graphics.Color;
with Sf; use Sf;
with Sf.Graphics.Texture; use Sf.Graphics.Texture;
with Sf.Graphics; use Sf.Graphics;

package body PyGamer.Screen is

   Pixel_Scale : constant := 3;

   XS, XE, YS, YE : Natural := 0;
   X, Y : Natural := 0;

   Debug_Enabled : Boolean := False;
   Debug_Color   : sfUInt32 := 100;

   generic
      with function Convert_Pix (Pix : UInt16) return sfColor;
   procedure Push_Pixels_Gen (Addr : System.Address; Len : Natural);

   function RGB565_To_Color (Pix : HAL.UInt16) return sfColor is
      R : constant UInt32 := UInt32 (Shift_Right (Pix, 11)) and 2#11111#;
      G : constant UInt32 := UInt32 (Shift_Right (Pix, 5)) and 2#111111#;
      B : constant UInt32 := UInt32 (Shift_Right (Pix, 0)) and 2#11111#;

      R8 : constant UInt32 := UInt32 (Float (R) * 255.0 / 31.0);
      G8 : constant UInt32 := UInt32 (Float (G) * 255.0 / 63.0);
      B8 : constant UInt32 := UInt32 (Float (B) * 255.0 / 31.0);
   begin
      return Sf.Graphics.Color.fromRGB (sfUint8 (R8 and 16#FF#),
                                        sfUint8 (G8 and 16#FF#),
                                        sfUint8 (B8 and 16#FF#));
   end RGB565_To_Color;

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
   end Set_Address;

   --------------------
   -- Start_Pixel_TX --
   --------------------

   procedure Start_Pixel_TX is
   begin
      null;
   end Start_Pixel_TX;

   ------------------
   -- End_Pixel_TX --
   ------------------

   procedure End_Pixel_TX is
   begin
      null;
   end End_Pixel_TX;

   ---------------------
   -- Push_Pixels_Gen --
   ---------------------

   procedure Push_Pixels_Gen (Addr : System.Address; Len : Natural) is
      Actual_Pixels : array (0 .. Natural (Width * Height - 1)) of sfColor
        with Address => Frame_Buffer'Address;

      Data : HAL.UInt16_Array (1 .. Len - 1) with Address => Addr;
   begin

      for Pix of Data loop
         if Debug_Enabled then
            Actual_Pixels (X + Y * Width) := fromInteger (16#FF_00_00_00# or Debug_Color);
         else
            Actual_Pixels (X + Y * Width) := Convert_Pix (Pix);
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
   end Push_Pixels_Gen;

   -----------------
   -- Push_Pixels --
   -----------------

   procedure Push_Pixels (Addr : System.Address; Len : Natural) is

      function Convert_Pix (Pix : HAL.UInt16) return sfColor is

         --  Byte swap is inverted in the PyGamer simulator
         Swap : constant UInt16 := Shift_Right (Pix and 16#FF00#, 8) or
           (Shift_Left (Pix, 8) and 16#FF00#);
      begin
         return RGB565_To_Color (Swap);
      end Convert_Pix;

      procedure Push is new Push_Pixels_Gen (Convert_Pix);
   begin
      Push (Addr, Len);
   end Push_Pixels;

   -----------------
   -- Push_Pixels --
   -----------------

   procedure Push_Pixels_Swap (Addr : System.Address; Len : Natural) is
      procedure Push is new Push_Pixels_Gen (RGB565_To_Color);
   begin
      Push (Addr, Len);
   end Push_Pixels_Swap;

   ------------
   -- Scroll --
   ------------

   procedure Scroll (Val : HAL.UInt8) is
   begin
      Scroll_Val := Val mod Width;
   end Scroll;

   ----------------
   -- Debug_Mode --
   ----------------

   procedure Debug_Mode (Enabled : Boolean) is
   begin
      Debug_Enabled := Enabled;
   end Debug_Mode;

   ---------------
   -- Start_DMA --
   ---------------

   procedure Start_DMA (Addr : System.Address; Len : Natural) is
   begin
      --  We don't really have a DMA here...
      Push_Pixels (Addr, Len);
   end Start_DMA;

   ---------------------
   -- Wait_End_Of_DMA --
   ---------------------

   procedure Wait_End_Of_DMA is null;

end PyGamer.Screen;
