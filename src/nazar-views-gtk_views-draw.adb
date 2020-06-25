with Gdk.Event;

with Cairo.Image_Surface;

with Nazar.Colors;
with Nazar.Draw_Operations;
with Nazar.Gtk_Main;
with Nazar.Main;
with Nazar.Trigonometry;

package body Nazar.Views.Gtk_Views.Draw is

   function From_Object is
     new Nazar.Views.Gtk_Views.From_Gtk_Object
       (Nazar_Gtk_Draw_View_Record, Nazar_Gtk_Draw_View);

   function Configure_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Draw_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Cr    : Cairo.Cairo_Context)
      return Boolean;

   procedure Clear
     (Surface : Cairo.Cairo_Surface;
      Color   : Nazar.Colors.Nazar_Color);

   type Cairo_Render_Type is
     new Nazar.Draw_Operations.Root_Render_Type with
      record
         Cr      : Cairo.Cairo_Context;
         X, Y    : Glib.Gdouble := 0.0;
         Moved   : Boolean := False;
      end record;

   overriding procedure Move_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float);

   overriding procedure Line_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float);

   overriding procedure Text
     (Render : in out Cairo_Render_Type;
      S      : String);

   overriding procedure Arc
     (Render        : in out Cairo_Render_Type;
      Radius        : Nazar_Float;
      Start_Radians : Nazar_Float;
      End_Radians   : Nazar_Float);

   overriding procedure Image
     (Render        : in out Cairo_Render_Type;
      Resource_Name : String;
      Width, Height : Nazar_Float;
      Rotation      : Nazar.Trigonometry.Angle);

   overriding procedure Render_Current
     (Render   : in out Cairo_Render_Type;
      Fill     : Boolean;
      Preserve : Boolean);

   overriding procedure Set_Color
     (Render : in out Cairo_Render_Type;
      Color  : Nazar.Colors.Nazar_Color);

   overriding procedure Set_Font
     (Render : in out Cairo_Render_Type;
      Family : String;
      Size   : Nazar_Float;
      Italic : Boolean;
      Bold   : Boolean);

   overriding procedure Save_State
     (Render : in out Cairo_Render_Type);

   overriding procedure Restore_State
     (Render : in out Cairo_Render_Type);

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (View  : not null access Nazar_Gtk_Draw_View_Record;
      Model : not null access Nazar.Models.Draw.Root_Draw_Model'Class)
   is
      use type Nazar.Models.Nazar_Model;
   begin
      if View.Base_Model = null then
         View.Set_Model (Model);
      else
         View.Layers.Append
           (Draw_Layer_Record'
              (Background  => False,
               Need_Render => True,
               Model       => Model_Access (Model),
               Surface     => Cairo.Null_Surface));
      end if;
   end Append;

   ---------
   -- Arc --
   ---------

   overriding procedure Arc
     (Render        : in out Cairo_Render_Type;
      Radius        : Nazar_Float;
      Start_Radians : Nazar_Float;
      End_Radians   : Nazar_Float)
   is
   begin
      Cairo.Arc (Render.Cr, Render.X, Render.Y,
                 Glib.Gdouble (Radius),
                 Glib.Gdouble (Start_Radians),
                 Glib.Gdouble (End_Radians));
   end Arc;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Surface : Cairo.Cairo_Surface;
      Color   : Nazar.Colors.Nazar_Color)
   is
      Cr : constant Cairo.Cairo_Context := Cairo.Create (Surface);
   begin
      Cairo.Set_Source_Rgba
        (Cr,
         Glib.Gdouble (Color.Red),
         Glib.Gdouble (Color.Green),
         Glib.Gdouble (Color.Blue),
         Glib.Gdouble (Color.Alpha));
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear;

   -----------------------
   -- Configure_Handler --
   -----------------------

   function Configure_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      use Glib;
      use type Cairo.Cairo_Surface;
      View   : constant Nazar_Gtk_Draw_View := From_Object (Self);
      Width  : constant Gint := Event.Width;
      Height : constant Gint := Event.Height;
      Reload : constant Boolean :=
                 Width /= View.Width or else Height /= View.Height;
   begin

      for Layer of View.Layers loop
         if Reload or else Layer.Surface = Cairo.Null_Surface then
            if Layer.Surface /= Cairo.Null_Surface then
               Cairo.Surface_Destroy (Layer.Surface);
            end if;

            Layer.Surface :=
              Cairo.Image_Surface.Create
                (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
                 Width  => Width,
                 Height => Height);
         end if;
      end loop;

      View.Width := Width;
      View.Height := Height;

      View.Redraw;

      return True;

   end Configure_Handler;

   ------------------
   -- Draw_Handler --
   ------------------

   function Draw_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Cr    : Cairo.Cairo_Context)
      return Boolean
   is
      View   : constant Nazar_Gtk_Draw_View := From_Object (Self);
   begin
      for Layer of View.Layers loop
         Cairo.Set_Source_Surface (Cr, Layer.Surface, 0.0, 0.0);
         Cairo.Paint (Cr);
      end loop;
      return True;
   end Draw_Handler;

   -----------
   -- Image --
   -----------

   overriding procedure Image
     (Render        : in out Cairo_Render_Type;
      Resource_Name : String;
      Width, Height : Nazar_Float;
      Rotation      : Nazar.Trigonometry.Angle)
   is
      use Glib;
      Image_Surface : constant Cairo.Cairo_Surface :=
                        Nazar.Gtk_Main.Get_Image_Resource (Resource_Name);
      Img_Height    : constant Gint :=
                        Cairo.Image_Surface.Get_Height (Image_Surface);
      Img_Width     : constant Gint :=
                        Cairo.Image_Surface.Get_Width (Image_Surface);
      Height_Ratio  : constant Gdouble :=
                        Gdouble (Height) / Gdouble (Img_Height);
      Width_Ratio   : constant Gdouble :=
                        Gdouble (Width) / Gdouble (Img_Width);
   begin
      Cairo.Save (Render.Cr);
      Cairo.Translate
        (Render.Cr,
         Render.X - Gdouble (Width / 2.0),
         Render.Y - Gdouble (Height / 2.0));
      Cairo.Scale (Render.Cr, Width_Ratio, Height_Ratio);
      Cairo.Set_Source_Surface (Render.Cr, Image_Surface, 0.0, 0.0);
      Cairo.Paint (Render.Cr);
      Cairo.Restore (Render.Cr);
   end Image;

   -------------
   -- Line_To --
   -------------

   overriding procedure Line_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float)
   is
   begin
      if not Render.Moved then
         Cairo.Move_To (Render.Cr, Render.X, Render.Y);
         Render.Moved := True;
      end if;
      Render.X := Glib.Gdouble (X);
      Render.Y := Glib.Gdouble (Y);
      Cairo.Line_To (Render.Cr, Render.X, Render.Y);
   end Line_To;

   -------------
   -- Move_To --
   -------------

   overriding procedure Move_To
     (Render : in out Cairo_Render_Type;
      X, Y   : Nazar_Float)
   is
   begin
      Render.X := Glib.Gdouble (X);
      Render.Y := Glib.Gdouble (Y);
      Render.Moved := False;
   end Move_To;

   --------------------------------
   -- Nazar_Gtk_Draw_View_Create --
   --------------------------------

   function Nazar_Gtk_Draw_View_Create
     return Nazar_View
   is
      View : constant Nazar_Gtk_Draw_View :=
        new Nazar_Gtk_Draw_View_Record;
   begin
      View.Draw_Area := Gtk.Drawing_Area.Gtk_Drawing_Area_New;
      View.Initialize (View.Draw_Area);
      View.Draw_Area.On_Configure_Event
        (Configure_Handler'Access, View.Object);
      View.Draw_Area.On_Draw
        (Draw_Handler'Access, View.Object);
      return Nazar_View (View);
   end Nazar_Gtk_Draw_View_Create;

   -----------------------------
   -- Nazar_Gtk_Draw_View_New --
   -----------------------------

   function Nazar_Gtk_Draw_View_New
     (Model : not null access Nazar.Models.Draw.Root_Draw_Model'Class)
      return Nazar_Gtk_Draw_View
   is
      View : constant Nazar_Gtk_Draw_View :=
        Nazar_Gtk_Draw_View (Nazar_Gtk_Draw_View_Create);
   begin
      View.Viewport := Model.Bounding_Box;
      View.Set_Model (Model);
      View.Layers.Append
        (Draw_Layer_Record'
           (Background  => True,
            Need_Render => True,
            Model       => Model_Access (Model),
            Surface     => Cairo.Null_Surface));
      return View;
   end Nazar_Gtk_Draw_View_New;

   ------------
   -- Redraw --
   ------------

   procedure Redraw
     (View : in out Nazar_Gtk_Draw_View_Record)
   is
      use type Nazar.Models.Nazar_Model;
   begin
      if View.Model = null then
         for Layer of View.Layers loop
            if Layer.Background then
               Clear (Layer.Surface, (0.6, 0.45, 0.82, 1.0));
            else
               Clear (Layer.Surface, (1.0, 0.2, 0.3, 0.0));
            end if;
         end loop;
      else
         for Layer of View.Layers loop
            if Layer.Need_Render
              or else Layer.Model.Render_Queued
            then
               declare
                  Context : Nazar.Draw_Operations.Draw_Context;
                  Render  : Cairo_Render_Type;
               begin
                  if Layer.Background then
                     Clear (Layer.Surface, Layer.Model.Background_Color);
                  else
                     Clear (Layer.Surface, (1.0, 1.0, 1.0, 0.0));
                  end if;

                  Render.Cr := Cairo.Create (Layer.Surface);
                  Nazar.Draw_Operations.Set_Target
                    (Context,
                     Nazar_Float (View.Width), Nazar_Float (View.Height));
                  Nazar.Draw_Operations.Set_Viewport (Context, View.Viewport);
                  Layer.Model.Render (Context, Render);
                  Cairo.Destroy (Render.Cr);
                  Layer.Need_Render := False;
               end;
            end if;
         end loop;
      end if;
   end Redraw;

   --------------------
   -- Render_Current --
   --------------------

   overriding procedure Render_Current
     (Render   : in out Cairo_Render_Type;
      Fill     : Boolean;
      Preserve : Boolean)
   is
   begin
      if Fill then
         if Preserve then
            Cairo.Fill_Preserve (Render.Cr);
         else
            Cairo.Fill (Render.Cr);
         end if;
      else
         if Preserve then
            Cairo.Stroke_Preserve (Render.Cr);
         else
            Cairo.Stroke (Render.Cr);
         end if;
      end if;
   end Render_Current;

   -------------------
   -- Restore_State --
   -------------------

   overriding procedure Restore_State
     (Render : in out Cairo_Render_Type)
   is
   begin
      Nazar.Draw_Operations.Root_Render_Type (Render).Restore_State;
      Cairo.Restore (Render.Cr);
   end Restore_State;

   ----------------
   -- Save_State --
   ----------------

   overriding procedure Save_State
     (Render : in out Cairo_Render_Type)
   is
   begin
      Nazar.Draw_Operations.Root_Render_Type (Render).Save_State;
      Cairo.Save (Render.Cr);
   end Save_State;

   ---------------
   -- Set_Color --
   ---------------

   overriding procedure Set_Color
     (Render : in out Cairo_Render_Type;
      Color  : Nazar.Colors.Nazar_Color)
   is
   begin
      Cairo.Set_Source_Rgba
        (Render.Cr,
         Glib.Gdouble (Color.Red),
         Glib.Gdouble (Color.Green),
         Glib.Gdouble (Color.Blue),
         Glib.Gdouble (Color.Alpha));
   end Set_Color;

   --------------
   -- Set_Font --
   --------------

   overriding procedure Set_Font
     (Render : in out Cairo_Render_Type;
      Family : String;
      Size   : Nazar_Float;
      Italic : Boolean;
      Bold   : Boolean)
   is
   begin
      Cairo.Select_Font_Face
        (Cr     => Render.Cr,
         Family => Family,
         Slant  =>
           (if Italic
            then Cairo.Cairo_Font_Slant_Italic
            else Cairo.Cairo_Font_Slant_Normal),
         Weight =>
           (if Bold
            then Cairo.Cairo_Font_Weight_Bold
            else Cairo.Cairo_Font_Weight_Normal));
      Cairo.Set_Font_Size (Render.Cr, Glib.Gdouble (Size));
   end Set_Font;

   ---------------
   -- Set_Model --
   ---------------

   overriding procedure Set_Model
     (View  : not null access Nazar_Gtk_Draw_View_Record;
      Model : not null access Nazar.Models.Nazar_Model_Record'Class)
   is
      Bounding_Box : constant Rectangle :=
        Nazar.Models.Draw.Nazar_Draw_Model (Model).Bounding_Box;
   begin
      View.Layers.Append
        (Draw_Layer_Record'
           (Background  => True,
            Need_Render => True,
            Model       => Model_Access (Model),
            Surface     => Cairo.Null_Surface));
      View.Viewport := Bounding_Box;
      Nazar.Views.Gtk_Views.Nazar_Gtk_View_Record (View.all).Set_Model (Model);
   end Set_Model;

   ------------------
   -- Set_Viewport --
   ------------------

   overriding procedure Set_Viewport
     (View     : in out Nazar_Gtk_Draw_View_Record;
      Viewport : Rectangle)
   is
   begin
      View.Viewport := Viewport;
   end Set_Viewport;

   ----------
   -- Text --
   ----------

   overriding procedure Text
     (Render : in out Cairo_Render_Type;
      S      : String)
   is
   begin
      if not Render.Moved then
         Cairo.Move_To (Render.Cr, Render.X, Render.Y);
         Render.Moved := True;
      end if;

      Cairo.Show_Text (Render.Cr, S);

      Render.Moved := False;

   end Text;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Draw_View_Record)
   is
   begin
      View.Redraw;
      View.Draw_Area.Queue_Draw;
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Draw;
