with HAL;
with HAL.Bitmap;

package PyGamer.Screen is

   Width  : constant := 160;
   Height : constant := 128;

   procedure Set_Address (X_Start, X_End, Y_Start, Y_End : HAL.UInt16);

   procedure Start_Pixel_TX;
   procedure End_Pixel_TX;

   procedure Push_Pixels (Data : aliased HAL.UInt16_Array);

   procedure Push_Pixels_Swap (Data : aliased in out HAL.UInt16_Array);

   procedure Scroll (Val : HAL.UInt8);

   procedure Debug_Mode (Enabled : Boolean);

   -- DMA --

   type Framebuffer_Access is access constant HAL.UInt16_Array;

   procedure Start_DMA (Data : not null Framebuffer_Access);
   procedure Wait_End_Of_DMA;

end PyGamer.Screen;
