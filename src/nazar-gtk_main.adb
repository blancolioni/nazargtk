with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with WL.String_Maps;

with Cairo.Png;

with Nazar.Paths;

package body Nazar.Gtk_Main is

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

   package Surface_Maps is
     new WL.String_Maps (Cairo.Cairo_Surface, Cairo."=");

   ------------
   -- Assets --
   ------------

   protected Assets is

      procedure Add_Image
        (Resource_Name : String;
         Surface       : Cairo.Cairo_Surface);

      procedure Get_Image_Resource
        (Resource_Name : String;
         Surface       : out Cairo.Cairo_Surface);

   private

      Default : Cairo.Cairo_Surface;
      Map     : Surface_Maps.Map;

   end Assets;

   ---------------
   -- Add_Image --
   ---------------

   procedure Add_Image
     (Resource_Name : String;
      File_Path     : String)
   is
   begin
      if not Ada.Directories.Exists (File_Path) then
         raise Constraint_Error with
           "image not found: " & File_Path;
      end if;

      declare
         Surface : constant Cairo.Cairo_Surface :=
                     Cairo.Png.Create_From_Png (File_Path);
      begin
         Assets.Add_Image (Resource_Name, Surface);
      end;
   end Add_Image;

   protected body Assets is

      ---------------
      -- Add_Image --
      ---------------

      procedure Add_Image
        (Resource_Name : String;
         Surface       : Cairo.Cairo_Surface)
      is
      begin
         if Map.Contains (Resource_Name) then
            Map.Replace (Resource_Name, Surface);
         else
            Map.Insert (Resource_Name, Surface);
         end if;
      end Add_Image;

      ------------------------
      -- Get_Image_Resource --
      ------------------------

      procedure Get_Image_Resource
        (Resource_Name : String;
         Surface       : out Cairo.Cairo_Surface)
      is
         use type Cairo.Cairo_Surface;
      begin
         if Map.Contains (Resource_Name) then
            Surface := Map.Element (Resource_Name);
         else
            if Default = Cairo.Null_Surface then
               Default :=
                 Cairo.Png.Create_From_Png
                   (Nazar.Paths.Config_File ("gtk/missing.png"));
            end if;
            Surface := Default;
         end if;
      end Get_Image_Resource;

   end Assets;

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

   ------------------------
   -- Get_Image_Resource --
   ------------------------

   function Get_Image_Resource
     (Resource_Name : String)
      return Cairo.Cairo_Surface
   is
   begin
      return Result : Cairo.Cairo_Surface do
         Assets.Get_Image_Resource (Resource_Name, Result);
      end return;
   end Get_Image_Resource;

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

end Nazar.Gtk_Main;
