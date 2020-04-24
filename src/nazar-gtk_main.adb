with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;

with Glib.Main;

package body Nazar.Gtk_Main is

   package User_Data_Holders is
     new Ada.Containers.Indefinite_Holders
       (Nazar.Signals.User_Data_Interface'Class,
        Nazar.Signals."=");

   Timer_User_Data : User_Data_Holders.Holder;
   Timer_Callback  : Timer_Handler;

   function On_Timer return Boolean;

   package View_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Nazar.Views.Gtk_Views.Nazar_Gtk_View,
        Nazar.Views.Gtk_Views."=");

   protected Scheduled_Views is

      procedure Schedule (View : Nazar.Views.Gtk_Views.Nazar_Gtk_View);
      procedure Get_Views (List : out View_Lists.List);

   private
      View_List : View_Lists.List;
   end Scheduled_Views;

   ---------------------
   -- Execute_Updates --
   ---------------------

   procedure Execute_Updates is
      List : View_Lists.List;
   begin
      Scheduled_Views.Get_Views (List);
      for View of List loop
         View.Update_From_Model;
      end loop;
   end Execute_Updates;

   --------------
   -- On_Timer --
   --------------

   function On_Timer return Boolean is
   begin
      Timer_Callback (Timer_User_Data.Element);
      return True;
   end On_Timer;

   ---------------------
   -- Schedule_Update --
   ---------------------

   procedure Schedule_Update
     (View : not null access Nazar.Views.Gtk_Views.Nazar_Gtk_View_Record'Class)
   is
   begin
      Scheduled_Views.Schedule
        (Nazar.Views.Gtk_Views.Nazar_Gtk_View (View));
   end Schedule_Update;

   protected body Scheduled_Views is

      ---------------
      -- Get_Views --
      ---------------

      procedure Get_Views (List : out View_Lists.List) is
      begin
         List := View_List;
         View_List.Clear;
      end Get_Views;

      --------------
      -- Schedule --
      --------------

      procedure Schedule
        (View : Nazar.Views.Gtk_Views.Nazar_Gtk_View)
      is
      begin
         if not View_List.Contains (View) then
            View_List.Append (View);
         end if;
      end Schedule;

   end Scheduled_Views;

   -----------------
   -- Start_Timer --
   -----------------

   procedure Start_Timer
     (Timeout   : Duration;
      User_Data : Nazar.Signals.User_Data_Interface'Class;
      Callback  : Timer_Handler)
   is
      Timeout_Id : constant Glib.Main.G_Source_Id :=
                     Glib.Main.Timeout_Add
                       (Glib.Guint (Timeout * 1000.0), On_Timer'Access);
   begin
      pragma Unreferenced (Timeout_Id);
      Timer_Callback := Callback;
      Timer_User_Data := User_Data_Holders.To_Holder (User_Data);
   end Start_Timer;

end Nazar.Gtk_Main;
