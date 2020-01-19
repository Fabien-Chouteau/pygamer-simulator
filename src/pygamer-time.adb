with Ada.Real_Time;               use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;

with Ada.Text_IO;

package body PyGamer.Time is

   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;

   Period_Ms : constant := 100;
   Event_Period : constant Time_Span := Milliseconds (Period_Ms);

   Subscribers : array (1 .. 10) of Tick_Callback := (others => null);

   Event : Ada.Real_Time.Timing_Events.Timing_Event;

   Start_Time : Ada.Real_Time.Time;

   procedure Initialize;

   protected Events is
      procedure Handler (Event : in out Timing_Event);
      function Clock return Time_Ms;

   private
      Clock_Ms : Time_Ms := 0 with Volatile;
   end Events;

   ------------
   -- Events --
   ------------

   protected body Events is

      -------------
      -- Handler --
      -------------

      procedure Handler (Event : in out Timing_Event) is
      begin
         Clock_Ms := Clock_Ms + Period_Ms;

         for Subs of Subscribers loop

            if Subs /= null then
               --  Call the subscriber
               Subs.all;
            end if;

         end loop;

         --  Re-trigger
         Set_Handler (Event, Event_Period, Events.Handler'Access);
      end Handler;

      -----------
      -- Clock --
      -----------

      function Clock return Time_Ms is
      begin
         return Clock_Ms;
      end Clock;
   end Events;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Start_Time := Ada.Real_Time.Clock;
      Set_Handler (Event, Event_Period, Events.Handler'Access);
   end Initialize;

   -----------
   -- Clock --
   -----------

   function Clock return Time_Ms
   is (Time_Ms (To_Duration
                (Ada.Real_Time.Clock - Start_Time) * 1000.0));

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Milliseconds : UInt64) is
   begin
      Delay_Until (Clock + Milliseconds);
   end Delay_Ms;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Wakeup_Time : Time_Ms) is
   begin
      delay until Start_Time + Milliseconds (Integer (Wakeup_Time));
   end Delay_Until;

   -----------------
   -- Tick_Period --
   -----------------

   function Tick_Period return Time_Ms is
   begin
      return Period_Ms;
   end Tick_Period;

   ---------------------
   -- Tick_Subscriber --
   ---------------------

   function Tick_Subscriber (Callback : not null Tick_Callback) return Boolean
   is
   begin
      for Subs of Subscribers loop
         if Subs = Callback then
            return True;
         end if;
      end loop;
      return False;
   end Tick_Subscriber;

   --------------------
   -- Tick_Subscribe --
   --------------------

   function Tick_Subscribe (Callback : not null Tick_Callback) return Boolean
   is
   begin
      for Subs of Subscribers loop
         if Subs = null then
            Subs := Callback;
            return True;
         end if;
      end loop;

      return False;
   end Tick_Subscribe;

   ----------------------
   -- Tick_Unsubscribe --
   ----------------------

   function Tick_Unsubscribe (Callback : not null Tick_Callback) return Boolean
   is
   begin
      for Subs of Subscribers loop
         if Subs = Callback then
            Subs := null;
            return True;
         end if;
      end loop;
      return False;
   end Tick_Unsubscribe;

   ---------------
   -- HAL_Delay --
   ---------------

   Delay_Instance : aliased PG_Delays;

   function HAL_Delay return not null HAL.Time.Any_Delays is
   begin
      return Delay_Instance'Access;
   end HAL_Delay;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   overriding
   procedure Delay_Microseconds
     (This : in out PG_Delays;
      Us   :        Integer)
   is
      pragma Unreferenced (This);
   begin
      Delay_Ms (UInt64 (Us / 1000));
   end Delay_Microseconds;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   overriding
   procedure Delay_Milliseconds
     (This : in out PG_Delays;
      Ms   :        Integer)
   is
      pragma Unreferenced (This);
   begin
      Delay_Ms (UInt64 (Ms));
   end Delay_Milliseconds;

   -------------------
   -- Delay_Seconds --
   -------------------

   overriding
   procedure Delay_Seconds (This : in out PG_Delays;
                            S    :        Integer)
   is
      pragma Unreferenced (This);
   begin
      Delay_Ms (UInt64 (S * 1000));
   end Delay_Seconds;

begin
   Initialize;
end PyGamer.Time;
