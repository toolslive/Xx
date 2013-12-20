class main = object (self)

  val _desktop = new XxDesktop.desktop
  val mutable _angle = 0.0
  initializer self#_init()

  val mutable _focus = None
  val mutable _count = 0
  val mutable _time = 0.0

  method keyboard_cb ~key ~x ~y =
    match key with
      | '\027' -> exit 0
      | _ ->
        begin
          match _focus with
          | None -> ()
          | Some c -> c#keyboard ~key ~x ~y
        end


  method idle_cb () =
    self#draw ();
    self#_update ()



  method draw () =
    GlClear.clear [`color; `depth];
    Gl.enable `blend;
    GlFunc.blend_func `src_alpha `one_minus_src_alpha;
    GlDraw.shade_model `smooth;
    GlMat.load_identity ();
    GlMat.rotate () ~angle:_angle ~y:0.0 ~z:(-. 1.0);

    _desktop # draw ();
    GlMat.push();
    GlMat.pop ();
    Gl.flush ();
    Glut.glutSwapBuffers ()

  method _update () =
    _angle <- _angle +. 0.01;
    _count <- _count + 1;
    if _count > 1000 then
      begin
        let time = Sys.time () in
        Printf.printf "fps = %f\n" (1000.0 /. (time -. _time));
        _count <- 0;
        _time <- time;
      end





  method _init() =
    (* make sure we have a theme *)
    let dt = new XxDefault.default_theme in
    let t = (dt :> XxComponent.theme) in
      XxComponent.set_theme t;

      let dtk = new XxDefault.default_toolkit in
      let tk = (dtk :> XxComponent.toolkit) in
      XxComponent.set_toolkit tk;

      let w1 = new XxWindow.window in
      let w1_c = (w1 :> XxComponent.component) in
      let b1 = new XxWindow.border w1_c in
      let b1_c = (b1 :> XxComponent.component) in
      _desktop#add_child b1_c;
      b1_c#set_pos 100.0 50.0;


      let w2 = new XxWindow.window in
      let w2_c = (w2 :> XxComponent.component) in
      w2_c#set_pos (-.300.0) (-.100.0);
      let tb = new XxWindow.titlebar w2_c "window" in
      let tb_c = (tb :> XxComponent.component) in

      _desktop#add_child tb_c;

      let cons = new XxConsole.console w1_c in
      _focus <- (Some (cons :> XxComponent.keyboard_handler));
      print_string w1#dump



  method reshape_cb ~width ~height =
    GlDraw.viewport ~x:0 ~y:0 ~w:width ~h:height;
    GlMat.mode `projection;
    GlMat.load_identity();
    GluMat.perspective
      ~fovy:60.0
      ~aspect:(float width /. float height)
      ~z:(1.0,20.0);
    GlMat.mode `modelview;
    GlMat.load_identity();
    GlMat.translate () ~z:(-5.0)

end

let myinit () =
  GlDraw.shade_model `smooth

open Glut
let main () =
  let
      width = 640 and
      height = 480
  in
  let m = new main
  in
    ignore (Glut.glutInit Sys.argv);
    Glut.glutInitDisplayMode [GLUT_ALPHA;GLUT_DEPTH;GLUT_DOUBLE];
    Glut.glutInitWindowSize width height;
    ignore (Glut.glutCreateWindow "GUI");
    Glut.glutDisplayFunc m#draw;
    Glut.glutKeyboardFunc m#keyboard_cb;
    Glut.glutReshapeFunc m#reshape_cb;
    Glut.glutIdleFunc (m#idle_cb);
    Glut.glutMainLoop ()

let _ = main ()
