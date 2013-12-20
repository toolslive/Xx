class point2 x y = object (self)
  val _x = x
  val _y = y

  method x = _x
  method y = _y

  method orthogonal =
    if _x = 0.0 then new point2 0.0 1.0
    else new point2  (-. _y /. _x) 1.0

  method length = sqrt(_x *. _x +. _y *. _y)

  method mult (p:point2) = _x *. p#x +. _y *. p#y

  method to_s = "(point2 " ^ (string_of_float _x) ^ " " ^ (string_of_float y) ^ ")"

end

class virtual renderer =
        object
          method virtual lineloop : (point2 list) -> unit
          method virtual triangle : (point2 list) -> unit
        end

class virtual glyph = object (self)
                        method virtual render : renderer -> unit
                        method virtual width: float
                      end

class virtual font = object(self)
                       method virtual getGlyph : int -> glyph
                       method virtual letterSpacing : float
                     end

let add_point2 (p:point2) (q:point2) = new point2 (p#x +. q#x) (p#y +. q#y);;
let min_point2 (p:point2) (q:point2) = new point2 (p#x -. q#x) (p#y -. q#y);;
let equ_point2 (p:point2) (q:point2) = (p#x = q#x) && (p#y = q#y);;
let dot_point2 (p:point2) (q:point2) = p#x *. q#x  +. p#y *. q#y;;

type iResult = COLLINEAR | NONE | INTERSECTION of float * point2

let intersection p1 p2 q1 q2 =
  let eps = 1.0e-8 in
  let a = min_point2 q2 q1 and
      b = min_point2 p2 p1 and
      c = min_point2 q1 p1 in
  let aOrt = a#orthogonal in
  let sN = dot_point2 c aOrt and
      sD = dot_point2 b aOrt in
  let bOrt = b#orthogonal in
  let tN = -. (dot_point2 c bOrt) and
      tD = dot_point2 a bOrt in
  if abs_float(sD) <  eps then
    if abs_float(sN) < eps then
      COLLINEAR
    else NONE
  else
    let s = sN /. sD
    and t = tN /. tD in
    if 0. <= s && s <= 1. && 0. <= t && t <= 1. then
      INTERSECTION (s , new point2 (p1#x +.  s *. b#x) (p1#y +. s *. b#y))
    else
      NONE

type sideT = LEFT | ON_IT | RIGHT

let side e0 e1 p =
  let dx10 = e1#x -. e0#x and
      dy10 = e1#y -. e0#y and
      dy = p#y -. e0#y and
      dx = p#x -. e0#x in
  let (a:float) = dy *. dx10 and
      (b:float) = dy10 *. dx in
  if (a = b) then ON_IT
  else if (a > b) then LEFT
  else RIGHT;;





class polygon (contour: point2 list) = object (self)
  val _contour = contour
  method points = _contour
  method numberOfPoints = List.length _contour

  method point i = List.nth _contour i

  method render (renderer:renderer) =
    renderer#lineloop _contour

  method isConvex (j:int) =
    let n:int = self#numberOfPoints in
    let i:int = (j - 1) mod n
    and k:int = (j + 1) mod n in
    let pi:point2 = self#point i
    and pj = self#point j
    and pk = self#point k in
    let dy_ki = pk#y -. pi#y
    and dy_ji = pj#y -. pi#y
    and dx_ki = pk#x -. pi#x
    and dx_ji = pj#x -. pi#x in
    let r = (dy_ki *. dx_ji) > (dy_ji *. dx_ki) in
    r

end

class triangle contour = object(self)
  inherit polygon contour


  method render (renderer:renderer) =
    renderer#triangle(_contour)
end



class virtual fontBuilder = object(self)

                            end


(* implementations start here *)

class twlGlyph = object(self)
  inherit glyph

  val _triangles = ref ([]:triangle list)
  val _width = ref 4.0
  method render r =
    let doOne (t:triangle) =
      r#triangle t#points in
    List.iter doOne !_triangles


  method setTriangles t =
    _triangles := t

  method width = !_width

  method setWidth w = _width := w
end

class twlFont = object(self)
  inherit font
  val _table = Hashtbl.create 256

  method getGlyph code =
    try
      let result = Hashtbl.find _table code
      in (result :> glyph)
    with Not_found ->
      Printf.printf "Not_found %d\n" code;
      raise Not_found

  method setGlyph (code:int) (glyph:twlGlyph) =
    Hashtbl.add _table code glyph
  method letterSpacing = 4.0
end



type state = POINTS | TRIANGLES | START

let _hex1 s =
  let os = int_of_char s in
  if os > 96 then  os - 87 else
    if os > 64 then  os - 55 else
      os - 48;;

let _hex2 s =
  let msd = _hex1 s.[0] and
      lsd = _hex1 s.[1] in
  msd * 16 + lsd;;


class twlFontBuilder = object(self)

  inherit fontBuilder


  method read (dirName:string) =
    let result = new twlFont in
    let level0s = Sys.readdir dirName in
    let doLevel level =
      let glyphFileNames = Sys.readdir (dirName ^ "/" ^ level) in
      let doGlyphFile fn =
        let resultGlyph = self#readGlyph dirName level fn and
            code = (_hex2 level * 256) + (_hex2 fn)
        in
        (* print_int code; *)
        begin
          result#setGlyph code resultGlyph;
        (*
           let codeS = string_of_int code in
           let widthS = string_of_float (resultGlyph#width) in
           print_string (codeS ^ ":" ^ widthS ^ "\n")
        *)
        end
      in
      Array.iter doGlyphFile glyphFileNames
    in
    Array.iter doLevel level0s;
    result

  method private readGlyph dn level fn =
    let glyph = new twlGlyph in
    let path = dn ^ "/" ^ level ^ "/" ^ fn in
    let input = open_in path in
    let s = ref START in
    let points = ref ([]: point2 list) in
    let triangles = ref ([]: triangle list) in
    let minX = ref 1000.0 in
    let maxX = ref (-.1000.0) in
    try
      while true do
        let line = input_line input in
        match !s with
        | POINTS ->
           if line.[0] = ']'
           then
             begin
               points := List.rev (!points);
               s := START;
               let width = max 0.0 (!maxX -. !minX) in
               glyph#setWidth width;
             end
           else
             let splitted = Str.split (Str.regexp " ") line in
             let floats = List.map float_of_string splitted in
             begin
               match floats with
               | x::y::_ ->
                  begin
                    minX := min !minX x;
                    maxX := max !maxX x;
                    points := (new point2 x y)::!points
                  end
               | _ -> ()
             end;
        | TRIANGLES ->
           if line.[0] = ']'
           then
             begin
               s := START;
               glyph#setTriangles (!triangles)
             end
           else
             begin
               let splitted = Str.split (Str.regexp " ") line in
               let ints = List.map int_of_string splitted in
               let i::j::k::_ = ints in
               let pi = List.nth !points i
               and pj = List.nth !points j
               and pk = List.nth !points k in
               let t = new triangle [pi;pj;pk] in
               triangles := t::(!triangles)
             end
        | START ->
           if line.[0] = 'p'
           then s:= POINTS
           else if line.[0] = 't'
           then s:= TRIANGLES
           else ()
      done;
      glyph
    with End_of_file -> glyph

end
