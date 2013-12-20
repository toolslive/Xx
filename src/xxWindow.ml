class border decorated = object(self)
  inherit XxComponent.decorator decorated

  method draw () =
    self#draw_self ();
    _component#draw () ;


  method draw_self () =
    let d = _component#get_dim in
    let w = d.(0)
    and h = d.(1)
    in
    let theme = XxComponent.get_theme () in
    let pos = _component#get_pos in
    let x = pos.(0)
    and y = pos.(1) in
      theme#draw_outline_rectangle (x,y,w,h);

  method get_dim =
    let d = _component#get_dim in
    let w = d.(0)
    and h = d.(1)
    and b2 = 2.0 *. 1.0
    in [| w+. b2; h +. b2|]

  method add_child = raise XxComponent.NoComposite

end

class scrollbar decorated = object(self)
  inherit XxComponent.decorator decorated

  val _width = 15.0

  method draw =
    GlMat.push();
    self#draw_self ();
    GlMat.pop();
    _component#draw;

  method draw_self () =
    let d = _component#get_dim in
    let x = d.(0)
    and y = d.(1)
    and w = _width
    in
      GlDraw.begins `quads;
      GlDraw.color (0.0,0.0,1.0);
      GlDraw.vertex ~x:x  ~y:0.0 ();
      GlDraw.color (0.0,1.0,1.0);
      GlDraw.vertex ~x:(x +. w) ~y:0.0 ();
      GlDraw.color (0.0,1.0,0.0);
      GlDraw.vertex ~x:(x +. w) ~y:y   ();
      GlDraw.color (0.0,0.0,0.0);
      GlDraw.vertex ~x:x        ~y:y   ();

      GlDraw.ends();

  method get_dim =
    let dim = _component#get_dim in
    let x = dim.(0)
    and y = dim.(1)
    in [|x +. _width; y|]

  method add_child = raise XxComponent.NoComposite

end

class titlebar decorated title = object(self)
	inherit XxComponent.decorator decorated

	val mutable _title = title
	val _textheight = 25.0

	method draw () =
	  GlMat.push();
	  self#draw_self ();
	  GlMat.pop();
	  _component#draw () ;


	method draw_self ()  =
	  let d = _component#get_dim in
	  let w = d.(0)
	  and h = d.(1)
	  in
	  let pos = _component#get_pos in
	  let x = pos.(0)
	  and	y = pos.(1)
	  in
	  let theme = XxComponent.get_theme () in
	    theme#draw_label (x,y+.h,w,_textheight) ~s:_title


	method get_dim =
	  let d = _component#get_dim in
	  let result = [|d.(0);d.(1)+. _textheight |] in
	    result

	method add_child = raise XxComponent.NoComposite

end


class window = object(self)
  inherit XxComponent.component

  val mutable _pos = [|0.0;0.0;0.0|]
  val mutable _dim = [|0.0;0.0|]
  val mutable _children = ([]:XxComponent.component list)

  initializer
    begin
      _dim.(0) <- 320.0;
      _dim.(1) <- 200.0
    end

  method pre_draw () =
    GlMat.translate  ~x: _pos.(0) ~y:_pos.(1) ~z:_pos.(2) ()

  method get_dim = _dim

  method add_child c =
    Printf.printf "Window::add_child %s" c#dump;
    _children <- c::_children

  method set_pos x y =
    _pos.(0) <- x;
    _pos.(1) <- y
  method get_pos = _pos

  method get_children = _children

  method draw_self () =
    self#_set_background_colour;

    let x = _dim.(0)
    and y = _dim.(1)
    in
    let theme = XxComponent.get_theme() in
      theme#fill_rectangle (0.0,0.0,x,y);


	method _set_background_colour =
		let theme = XxComponent.get_theme () in
		let scheme = theme#get_scheme XxComponent.WINDOW  in
		let color = scheme#background_colour in
		let r,g,b,a = color#get_rgba in
		GlDraw.color (r,g,b) ~alpha:a


	method dump =
		let result = Buffer.create 30 in
		Buffer.add_string result "<window x=\"";
		Buffer.add_string result (string_of_float _pos.(0));
		Buffer.add_string result "\" y=\"";
		Buffer.add_string result (string_of_float _pos.(1));
		Buffer.add_string result "\" z=\"";
		Buffer.add_string result (string_of_float _pos.(2));
		Buffer.add_string result "\">";
		let f = fun x -> Buffer.add_string result x#dump in
		List.iter f self#get_children;
		Buffer.add_string result "</window>";
		Buffer.contents result
end
