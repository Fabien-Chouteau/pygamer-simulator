with Interfaces;

package PyGamer.Audio is

   type Data_Array is array (Natural range <>) of aliased Interfaces.Unsigned_16;
   type Audio_Callback is access procedure (Left, Right : out Data_Array);

   type Sample_Rate_Kind is (SR_11025, SR_22050, SR_44100, SR_96000);

   procedure Set_Callback (Callback    : Audio_Callback;
                           Sample_Rate : Sample_Rate_Kind);

end PyGamer.Audio;
