exception NoParent
exception NoFont
exception NoTheme
exception NoToolkit
exception NoRenderer
exception NoComposite

class virtual scheme = object
  method virtual border_colour: XxColour.colour

  method virtual background_colour: XxColour.colour

end

type component_type = DESKTOP | WINDOW | DUNNO
type rectangle = float*float*float*float

class virtual keyboard_handler = object
  method virtual keyboard: key:char -> x:int -> y:int -> unit

end

class virtual toolkit = object(self)

  method virtual draw_char : char -> unit
  method virtual char_width : char -> float

  method virtual draw_string : ?mw:float -> string -> unit
  method virtual string_width : string -> float

  (* GL stuff. here ?
     method push : unit
     method pop : unit
  *)
end


class virtual theme = object
  method virtual get_scheme: component_type -> scheme
  method virtual border_width: float
  method virtual draw_outline_rectangle: rectangle -> unit
  method virtual draw_label: rectangle -> s:string -> unit
  method virtual fill_rectangle: rectangle -> unit

end

(* singleton like access to the one theme *)

let global_theme = ref (None: theme option)
let global_toolkit = ref (None: toolkit option)

let get_theme () =
  match !global_theme with
      None -> raise NoTheme
    |   Some t -> t

let get_toolkit () =
  match !global_toolkit with
      None -> raise NoToolkit
    |   Some t -> t

let set_theme t =
  global_theme := Some t

let set_toolkit k =
  global_toolkit := Some k



class virtual component = object(self)
  method pre_draw () = ()

  method draw () =
    self#pre_draw () ;
    self#draw_self () ;
    let f = fun x ->
      begin
	GlMat.push();
        x#draw () ;
        GlMat.pop();
      end
    in
	List.iter f self#get_children;
	self#post_draw () ;

  method post_draw () = ()

  method get_children = ([]: component list)
  method virtual add_child: component -> unit


  method virtual draw_self: unit -> unit
  method virtual get_dim : float array

  method virtual set_pos : float -> float -> unit
  method virtual get_pos : float array
  method dump = "<component/>"
end

class virtual decorator (component:component)= object(self)
  inherit component

  val _component = component
  method draw = _component#draw

  method get_pos = _component#get_pos
  method set_pos = _component#set_pos
end
