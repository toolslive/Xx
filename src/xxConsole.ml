class console (parent: XxComponent.component) = object(self)
  inherit XxComponent.component
  inherit XxComponent.keyboard_handler

  val mutable _parent = parent
  initializer
    begin

      print_string "console::set_parent";
      parent#add_child (self:> XxComponent.component);
      self#_clear ()
    end

  val _buffer = String.create 80
  val mutable _lines = ([]:string list)
  val mutable _cursor = 0 (* position *)
  val _max_width = 320.0
  val _max_height = 175.0
  val _scale = 2.0
  val _line_height = 12.0



  method keyboard ~key ~x ~y =
    (*
      print_string "char_of(";
      print_int key;
      Printf.printf ")=%c\n" (char_of_int key);
    *)
    print_char key;
    print_string "\n";
    match key with
      | '\n' | '\r' (* return *)-> self#_new_line ()
      | '\008'  when _cursor > 0 ->
	  _cursor <- _cursor -1;
	  _buffer.[_cursor] <- ' '
      | a -> self#_char key

  method _char c=
    if _cursor < 80 then
      begin
	_buffer.[_cursor] <- c;
	_cursor <- _cursor + 1
      end
    else
      begin
      end




  method draw_self () =
    GlMat.push ();
    GlMat.scale ~x:_scale ~y:_scale ~z:_scale ();
    GlMat.translate () ~y:3.0; (* room at the bottom *)
    let toolkit = XxComponent.get_toolkit () in
    toolkit#draw_string ~mw:(_max_width /. _scale)  _buffer;

    let rec print_inner lines y=
      match lines with
      | [] -> ()
      | h::t ->
        let height = _line_height +. (y *. _scale) in
        if height > _max_height then ()
        else
          begin
            GlMat.translate ~y:_line_height ();
            toolkit#draw_string ~mw:(_max_width /. _scale) h;
            print_inner t (y +. _line_height);
          end
    in
    print_inner _lines 0.0;
    GlMat.pop();
    self#_draw_cursor ()


  method _draw_cursor () =
    GlMat.push ();
    let tk = XxComponent.get_toolkit () in
    let s = String.sub _buffer 0 _cursor in
    let deltaX = tk#string_width s in
    GlMat.translate () ~y:1.5 ~x:deltaX;
    GlDraw.begins `line_loop;
    let w = 12.0 in
    let h = 4.0 in
    GlDraw.color (1.0,0.0,0.0);
    GlDraw.vertex () ~x:0.0 ~y:0.0 ~z:0.0;
    GlDraw.vertex () ~x:w   ~y:0.0 ~z:0.0;
    GlDraw.vertex () ~x:w   ~y:h   ~z:0.0;
    GlDraw.vertex () ~x:0.0 ~y:h   ~z:0.0;
    GlDraw.ends ();
    GlMat.pop()


  method _new_line () =
    let line = String.create (_cursor) in
      String.blit _buffer 0 line 0 _cursor;
      _lines <- line::_lines;
      (*
	let spell s =
	let print c =
	print_int (int_of_char c);
	print_string " "
	in String.iter print s;
	print_string "\n"
	in
	List.iter spell _lines;
      *)
      self# _clear ()

  method _clear ()=
    let rec clear index =
      match index with
      | 80 -> ()
      | _ -> _buffer.[index] <- ' '; clear (index+1)
    in clear 0;
    _cursor <- 0

  method get_dim = _parent#get_dim

  method add_child = raise XxComponent.NoComposite
  method dump = "<console/>"
  method set_pos x y = ()
  method get_pos = _parent#get_pos
end
