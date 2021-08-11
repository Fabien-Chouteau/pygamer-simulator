with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with System;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with Sf.Audio; use Sf.Audio;
with Sf.Audio.SoundStream; use Sf.Audio.SoundStream;
with Sf; use Sf;

package body PyGamer.Audio is

   Stream : sfSoundStream_Ptr;
   User_Callback : Audio_Callback := null
     with Atomic, Volatile;

   Stream_Data : array (1 .. 1024) of aliased sfInt16;

   function SFML_Audio_Callback (chunk  : access sfSoundStreamChunk;
                                 Unused : System.Address)
                                 return sfBool
     with Convention => C;

   function Convert (Sample : Unsigned_16) return sfInt16
   is (sfInt16 (Integer_32 (Sample) - 32_768));


   -------------------------
   -- SFML_Audio_Callback --
   -------------------------

   function SFML_Audio_Callback (chunk  : access sfSoundStreamChunk;
                                 Unused : System.Address)
                                 return sfBool
   is
      Len : constant Natural := Stream_Data'Length;
      Left : Data_Array (1 .. Integer (Len) / 2);
      Right : Data_Array (1 .. Integer (Len) / 2);
      Stream_Index : Natural := Stream_Data'First;

      CB : Audio_Callback := User_Callback;
   begin
      if CB /= null then
         CB (Left, Right);

         for Index in Left'Range loop
            Stream_Data (Stream_Index) := Convert (Left (Index));
            Stream_Data (Stream_Index + 1) := Convert (Right (Index));
            Stream_Index := Stream_Index + 2;
         end loop;
      else
         Stream_Data := (others => 0);
      end if;

      chunk.Samples := Stream_Data (Stream_Data'First)'access;
      chunk.NbSamples := Stream_Data'Length;
      return True;
   end SFML_Audio_Callback;

   ------------------
   -- Set_Callback --
   ------------------

   procedure Set_Callback (Callback    : Audio_Callback;
                           Sample_Rate : Sample_Rate_Kind) is
   begin
      User_Callback := Callback;

      if Stream /= null then
         Stop (Stream);
         destroy (Stream);
      end if;

      Stream := create (onGetData    => SFML_Audio_Callback'Access,
                        onSeek       => null,
                        channelCount => 2,
                        sampleRate   => (case Sample_Rate is
                                            when SR_11025 => 11_025,
                                            when SR_22050 => 22_050,
                                            when SR_44100 => 44_110,
                                            when SR_96000 => 96_000),
                        userData     => System.Null_Address);

      if Stream = null then
         Put_Line ("Could not create audio stream");
         GNAT.OS_Lib.OS_Exit (1);
      else
         play (Stream);
      end if;

   end Set_Callback;

begin
   if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then
      --  Select driver for openal on Windows
      GNAT.OS_Lib.Setenv ("ALSOFT_DRIVERS", "dsound");
   end if;
end PyGamer.Audio;
