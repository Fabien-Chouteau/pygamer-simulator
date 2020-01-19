with HAL;
with HAL.Bitmap;

package PyGamer.Screen is

   Width  : constant := 160;
   Height : constant := 128;

   procedure Set_Address (X_Start, X_End, Y_Start, Y_End : HAL.UInt16);

   procedure Start_Pixel_TX;
   procedure End_Pixel_TX;

   procedure Push_Pixels (Data : HAL.UInt16_Array);

   procedure Push_Pixels_Swap (Data : in out HAL.UInt16_Array);

   procedure Scroll (Val : HAL.UInt8);

   procedure Debug_Mode (Enabled : Boolean);

end PyGamer.Screen;
