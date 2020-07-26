with System;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with GNAT.OS_Lib;

package body PyGamer.Audio is

   User_Callback : Audio_Callback := null;

   type SDL_Data_Array is array (Natural) of Integer_16;
   type SDL_Data_Array_Access is access all SDL_Data_Array;

   procedure SDL_Audio_Callback (Userdata : System.Address;
                                 Stream   : SDL_Data_Array_Access;
                                 Len      : Interfaces.C.int);
   pragma Export (C, SDL_Audio_Callback, "sdl_audio_callback");

   function Init_SDL_Audio (Sample_Rate : Interfaces.C.int)
                            return Interfaces.C.int;
   pragma Import (C, Init_SDL_Audio, "init_sdl_audio");

   function Convert (Sample : Unsigned_16) return Integer_16
   is (Integer_16 (Integer_32 (Sample) - 32_768));

   ------------------------
   -- SDL_Audio_Callback --
   ------------------------

   procedure SDL_Audio_Callback (Userdata : System.Address;
                                 Stream   : SDL_Data_Array_Access;
                                 Len      : Interfaces.C.int)
   is
      Stream_Index : Natural := Stream'First;
      Left : Data_Array (Stream'First .. Integer (Len) / 4);
      Right : Data_Array (Stream'First .. Integer (Len) / 4);
   begin
      if User_Callback /= null then
         User_Callback (Left, Right);

         for Index in Left'Range loop
            Stream (Stream_Index) := Convert (Left (Index));
            Stream (Stream_Index + 1) := Convert (Right (Index));
            Stream_Index := Stream_Index + 2;
         end loop;
      else
         Stream (Stream'First .. Integer (Len) / 2) := (others => 0);
      end if;

   end SDL_Audio_Callback;

   ------------------
   -- Set_Callback --
   ------------------

   procedure Set_Callback (Callback    : Audio_Callback;
                           Sample_Rate : Sample_Rate_Kind) is
   begin
      User_Callback := Callback;

      if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then -- Memory leak right here...
         GNAT.OS_Lib.Setenv ("SDL_AUDIODRIVER", "directsound");
      end if;

      if Init_SDL_Audio (case Sample_Rate is
                            when SR_11025 => 11_025,
                            when SR_22050 => 22_050,
                            when SR_44100 => 44_110,
                            when SR_96000 => 96_000) /= 0
      then
         raise Program_Error with "SDL Audio init failed";
      end if;
   end Set_Callback;
end PyGamer.Audio;
