with Nazar.Signals;

with Nazar.Views.Gtk_Views;

package Nazar.Gtk_Main is

   procedure Schedule_Update
     (View : not null access
        Nazar.Views.Gtk_Views.Nazar_Gtk_View_Record'Class);

   procedure Execute_Updates;

   type Timer_Handler is access
     procedure (User_Data : Nazar.Signals.User_Data_Interface'Class);

   procedure Start_Timer
     (Timeout   : Duration;
      User_Data : Nazar.Signals.User_Data_Interface'Class;
      Callback  : Timer_Handler);

end Nazar.Gtk_Main;
