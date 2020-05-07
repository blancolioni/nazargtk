--  with Glib.Error;
with Glib.Main;

with Gdk.Display;
with Gdk.Screen;

with Gtk.Css_Provider;
with Gtk.Main;
with Gtk.Style_Context;
with Gtk.Widget;

with Nazar.Gtk_Main;

package body Nazar.Views.Gtk_Views.Application is

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Update_Views return Boolean;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (View  : in out Nazar_Gtk_Application_View_Record;
      Child :        not null access Nazar.Views.Nazar_View_Record'Class)
   is
   begin
      View.Window.Add
        (Nazar.Views.Gtk_Views.Nazar_Gtk_View (Child).Widget);
   end Append;

   ------------------------
   -- Declare_Properties --
   ------------------------

   overriding procedure Declare_Properties
     (View  : in out Nazar_Gtk_Application_View_Record)
   is
   begin
      Nazar_Gtk_View_Record (View).Declare_Properties;
      View.Declare_Property ("theme", "");
   end Declare_Properties;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   ---------------------------------------
   -- Nazar_Gtk_Application_View_Create --
   ---------------------------------------

   function Nazar_Gtk_Application_View_Create return Nazar.Views.Nazar_View is
      View : constant Nazar_Gtk_Application_View :=
        new Nazar_Gtk_Application_View_Record;
   begin
      Gtk.Main.Init;
      Gtk.Window.Gtk_New (View.Window);
      View.Initialize (View.Window);
      View.Window.On_Destroy (Destroy_Handler'Access);

      return Nazar_View (View);
   end Nazar_Gtk_Application_View_Create;

   ------------------------------------
   -- Nazar_Gtk_Application_View_New --
   ------------------------------------

   function Nazar_Gtk_Application_View_New
     (Model : not null access Nazar.Models.Application
        .Nazar_Application_Model_Record'
        Class)
      return Nazar_Gtk_Application_View
   is
      View : constant Nazar_View :=
        Nazar_Gtk_Application_View_Create;
   begin
      View.Set_Model (Model);
      return Nazar_Gtk_Application_View (View);
   end Nazar_Gtk_Application_View_New;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (View  : in out Nazar_Gtk_Application_View_Record)
   is
   begin
      Nazar_Gtk_View_Record (View).Show;

      declare
         use Gtk.Css_Provider;
         --  Error      : aliased Glib.Error.GError;
         Theme_Name : constant String := View.Get_Property ("theme", "");
         Have_Theme : constant Boolean :=
           Theme_Name /= "";
         Theme      : constant Gtk.Css_Provider.Gtk_Css_Provider :=
           (if Have_Theme
            then Gtk.Css_Provider.Get_Named (Theme_Name)
            else null);
         Override   : constant Gtk.Css_Provider.Gtk_Css_Provider :=
           Gtk.Css_Provider.Gtk_Css_Provider_New;
         Display    : constant Gdk.Display.Gdk_Display :=
           Gdk.Display.Get_Default;
         Screen     : constant Gdk.Screen.Gdk_Screen :=
           Gdk.Screen.Get_Default_Screen (Display);
      begin

         Gtk.Style_Context.Add_Provider_For_Screen
           (Screen   => Screen,
            Provider => +Theme,
            Priority => 600);

         Gtk.Style_Context.Add_Provider_For_Screen
           (Screen   => Screen,
            Provider => +Override,
            Priority => 700);

      end;

      declare
         Timeout_Id : constant Glib.Main.G_Source_Id :=
           Glib.Main.Timeout_Add (100, Update_Views'Access);
      begin
         pragma Unreferenced (Timeout_Id);
      end;

      View.Window.Maximize;
      View.Window.Show_All;

      Gtk.Main.Main;
   end Show;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Application_View_Record)
   is
   begin
      null;
   end Update_From_Model;

   ------------------
   -- Update_Views --
   ------------------

   function Update_Views return Boolean is
   begin
      Nazar.Gtk_Main.Execute_Updates;
      return True;
   end Update_Views;

end Nazar.Views.Gtk_Views.Application;
