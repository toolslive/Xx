class default_scheme = object(self)
  inherit XxComponent.scheme

  val _border_colour = new XxColour.colour (1.0,0.0,0.0,0.3)
  val _background_colour = new XxColour.colour (0.0,0.0,0.0,0.3)
  val _foreground_colour = new XxColour.colour (1.0,1.0,1.0,1.0)

  method border_colour = _border_colour
  method background_colour = _background_colour
  method foreground_colour = _foreground_colour

end

exception Unimplemented
class properties = object(self)
  method get_property = "todo"
end

class default_toolkit = object(self)
  inherit XxComponent.toolkit

  val mutable _renderer = None
  val _font:XxFont.font =
    let twlf =(new XxFont.twlFontBuilder)#read "DarkGardenMK"
    in
    (twlf:> XxFont.font)

  method draw_string ?(mw=10000.0) s =
    let r = self#getRenderer in
    r#render_string ~mw:mw s;
    ()

  method string_width (s:string) =
    let r = self#getRenderer in
    r#string_width s


  method draw_char (c:char) = raise Unimplemented

  method private getRenderer =
    let r =
      match _renderer with
      | None ->
         let x = new XxOgl_renderer.oglRenderer _font
         in _renderer <- Some x; x
      | Some x -> x
    in (r :> XxOgl_renderer.oglRenderer);


  method char_width c=
    let code = int_of_char c in
    let glyph = _font#getGlyph code in
    glyph#width


end

class default_theme = object(self)
  inherit XxComponent.theme


  initializer self#_init ()


  val _scheme = (new default_scheme :> XxComponent.scheme)

  method _init () = ()

  method get_scheme (ct:XxComponent.component_type) = _scheme

  method border_width = 2.0

  method private _set_colour c =
    let red,green,blue,alpha = c#get_rgba in
    GlDraw.color (red,green,blue) ~alpha:alpha

  method draw_outline_rectangle r =
    let x,y,w,h = r in
    let b = self#border_width in
    let x1 = x -. b
    and y1 = y -. b
    and x2 = x +. w +. b
    and y2 = y +. h +. b
    in
    let colour = _scheme#border_colour in
    self#_set_colour colour;
    GlDraw.begins `line_loop;

    GlDraw.vertex ~x:x1 ~y:y1 ();
    GlDraw.vertex ~x:x2 ~y:y1 ();
    GlDraw.vertex ~x:x2 ~y:y2 ();
    GlDraw.vertex ~x:x1 ~y:y2 ();
    GlDraw.ends ();


  (*
    Printf.printf "default_look::print_string (%s)\n" s;
    GlList.call_lists ~base:font_base (`byte s)
   *)

  method draw_label r ~s =
    let x,y,w,h = r in
    GlMat.translate ~x:x ~y:y ();
    GlDraw.begins `quads;
    GlDraw.color (1.0,0.0,0.0);
    GlDraw.vertex ~x:0.0 ~y:0.0 ();
    GlDraw.vertex ~x:0.0 ~y:h ();
    GlDraw.color (0.0,1.0,0.0);
    GlDraw.vertex ~x:w   ~y:h ();
    GlDraw.vertex ~x:w   ~y:0.0 ();
    GlDraw.ends();
    GlMat.translate ~x:4.0 ~y:4.0 ();
    let sc = 2.0 in
    GlMat.scale ~x:sc ~y:sc ~z:sc ();
    let tk = XxComponent.get_toolkit () in
    tk#draw_string  s

  method fill_rectangle r =
    let x,y,w,h = r in

    GlDraw.begins `quads;
    let x2 = x+.w
    and y2 = y+.h in

    GlDraw.vertex () ~x:x ~y:y;
    GlDraw.vertex () ~x:x2 ~y:y;
    GlDraw.vertex () ~x:x2 ~y:y2;
    GlDraw.vertex () ~x:x ~y:y2;
    GlDraw.ends()

  method to_string =
    "default_look"
end
