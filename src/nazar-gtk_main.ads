with Cairo;

with Nazar.Views.Gtk_Views;

package Nazar.Gtk_Main is

   procedure Schedule_Update
     (View : not null access
        Nazar.Views.Gtk_Views.Nazar_Gtk_View_Record'Class);

   procedure Execute_Updates;

   procedure Add_Image
     (Resource_Name : String;
      File_Path     : String);

   function Get_Image_Resource
     (Resource_Name : String)
      return Cairo.Cairo_Surface;

end Nazar.Gtk_Main;
