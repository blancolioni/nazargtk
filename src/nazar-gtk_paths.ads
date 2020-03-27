package Nazar.Gtk_Paths is

   Config_Path : constant String :=
     "C:\Users\fwilson\Documents\GitHub\nazar-gtk\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Nazar.Gtk_Paths;
