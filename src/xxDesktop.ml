class desktop = object (self)
  inherit XxComponent.component

  val mutable _children = []
  val _dim = [|800.0;600.0|]

  method add_child (c:XxComponent.component) =
    _children <- c::_children

  method get_children = _children

  method get_dim = _dim

  method pre_draw () =
    GlMat.translate () ~z:(-. 4.0);
    let s = 0.007 in
      GlMat.scale () ~x:s ~y:s ~z:s;

  method draw_self () =

    GlDraw.begins `quads;
    GlDraw.color (0.0,0.0,0.0);
    let z = 1.0 in
      GlDraw.vertex3 (400.0,(300.0),z);
      GlDraw.color (0.0,0.0,1.0);
      GlDraw.vertex3 (400.0,(-300.0),z);
      GlDraw.color (0.0,1.0,1.0);
      GlDraw.vertex3 ((-. 400.0),(-. 300.0),z);
      GlDraw.color (0.0,1.0,0.0);
      GlDraw.vertex3 ((-. 400.0),(300.0),z);
      GlDraw.ends();

  method set_pos x y= ()

  method get_pos = [|0.0;0.0|]
end
