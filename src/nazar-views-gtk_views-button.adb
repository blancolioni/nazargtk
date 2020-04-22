with Nazar.Values;

package body Nazar.Views.Gtk_Views.Button is

   function From_Object is
     new Nazar.Views.Gtk_Views.From_Gtk_Object
       (Nazar_Gtk_Button_View_Record, Nazar_Gtk_Button_View);

   procedure On_Button_Click
     (Self  : access Glib.Object.GObject_Record'Class);

   procedure On_Text_Property_Update
     (Properties : in out Nazar.Interfaces.Properties
      .Property_Container_Interface'Class;
      New_Value  : Nazar.Values.Nazar_Value);

   ------------------------
   -- Declare_Properties --
   ------------------------

   overriding procedure Declare_Properties
     (View : in out Nazar_Gtk_Button_View_Record)
   is
   begin
      Nazar_Gtk_View_Record (View).Declare_Properties;
      View.Declare_Property
        ("text", "", On_Text_Property_Update'Access);
   end Declare_Properties;

   ----------------------------------
   -- Nazar_Gtk_Button_View_Create --
   ----------------------------------

   function Nazar_Gtk_Button_View_Create
     return Nazar_View
   is
      View : constant Nazar_Gtk_Button_View :=
               Nazar_Gtk_Button_View_New
                 (Nazar.Models.Text.Nazar_Text_Model_New (""));
   begin
      return Nazar_View (View);
   end Nazar_Gtk_Button_View_Create;

   -------------------------------
   -- Nazar_Gtk_Button_View_New --
   -------------------------------

   function Nazar_Gtk_Button_View_New
     (Model : not null access Nazar.Models.Text.Nazar_Text_Model_Record'Class)
      return Nazar_Gtk_Button_View
   is
      Button : constant Gtk.Button.Gtk_Button :=
                 Gtk.Button.Gtk_Button_New_With_Label
                   (Model.Get_Text);
   begin
      return Result : constant Nazar_Gtk_Button_View :=
        new Nazar_Gtk_Button_View_Record
      do
         Result.Button := Button;
         Result.Initialize (Result.Button);
         Result.Set_Model (Model);

         Button.On_Clicked
           (On_Button_Click'Access, Result.Object);
      end return;
   end Nazar_Gtk_Button_View_New;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Self  : access Glib.Object.GObject_Record'Class)
   is
      View : constant Nazar_Gtk_Button_View := From_Object (Self);
   begin
      View.Emit_Activate_Signal;
   end On_Button_Click;

   -----------------------------
   -- On_Text_Property_Update --
   -----------------------------

   procedure On_Text_Property_Update
     (Properties : in out Nazar.Interfaces.Properties
      .Property_Container_Interface'Class;
      New_Value  : Nazar.Values.Nazar_Value)
   is
      View      : Nazar_Gtk_Button_View_Record renames
                    Nazar_Gtk_Button_View_Record (Properties);
      New_Label : constant String :=
                    Nazar.Values.To_String (New_Value);
   begin
      View.Button_Model.Set_Text (New_Label);
   end On_Text_Property_Update;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Button_View_Record)
   is
   begin
      View.Button.Set_Label (View.Button_Model.Get_Text);
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Button;
