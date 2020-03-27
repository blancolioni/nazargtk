with Nazar.Controllers.Console;
with Nazar.Models.Console;
with Nazar.Views.Console;

with Nazar.Builder;
with Nazar.Main;

with Nazar.Models.Environment;
with Nazar.Models.Directories;

with Nazar.Builder.Gtk_Creator;

with Nazar.Gtk_Paths;

procedure Nazar.Gtk_Driver is
   Controller : Nazar.Controllers.Console.Nazar_Console_Controller_Record;
   Model      : constant Nazar.Models.Console.Nazar_Console_Model :=
     new Nazar.Models.Console.Root_Console_Model;
   Env        : constant Nazar.Models.Environment.Nazar_Environment_Model :=
     new Nazar.Models.Environment.Root_Environment_Model;

   Builder : constant Nazar.Builder.Nazar_Builder :=
     Nazar.Builder.Nazar_Builder_New
       (Creator     => Nazar.Builder.Gtk_Creator.Get_Gtk_Creator,
        Config_Path => Nazar.Gtk_Paths.Config_File ("nazar-gtk.config"));
   App : constant Nazar.Views.Nazar_View := Builder.Get_View ("app");

begin

   Nazar.Main.Init;

   Model.Initialize
     (Root          =>
        Nazar.Models.Directories.Directory_Model
          (Nazar.Gtk_Paths.Config_Path),
      Environment   => Env,
      Default_Scope => "/");

   Controller.Start_Console
     (Model => Model,
      View  => Nazar.Views.Console.Nazar_Console_View
        (Builder.Get_View ("console")));

   App.Show;

   Nazar.Main.Stop;

end Nazar.Gtk_Driver;
