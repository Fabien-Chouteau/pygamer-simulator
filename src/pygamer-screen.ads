with System;
with HAL;
with HAL.Bitmap;

package PyGamer.Screen is

   Width  : constant := 160;
   Height : constant := 128;

   procedure Set_Address (X_Start, X_End, Y_Start, Y_End : HAL.UInt16);

   procedure Start_Pixel_TX;
   procedure End_Pixel_TX;

   procedure Push_Pixels (Addr : System.Address; Len : Natural);
   --  Addr: pointer to a read-only buffer of Len pixels (16-bit RGB565)

   procedure Push_Pixels_Swap (Addr : System.Address; Len : Natural);
   --  Addr: pointer to a read-write buffer of Len pixels (16-bit RGB565)

   procedure Scroll (Val : HAL.UInt8);

   procedure Debug_Mode (Enabled : Boolean);

   -- DMA --

   procedure Start_DMA (Addr : System.Address; Len : Natural);
   --  Addr: pointer to a read-only buffer of Len pixels (16-bit RGB565)

   procedure Wait_End_Of_DMA;

end PyGamer.Screen;
