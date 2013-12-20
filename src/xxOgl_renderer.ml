open XxFont

type displaylists =
	NotInitialized | DisplayLists of GlList.base


class oglRenderer (f:font)=
  object(self)
	inherit renderer
	val _f = f

	method private _renderGlyph (glyph:glyph) =
	  glyph#render (self :> renderer)

	  (*
	  GlDraw.begins `lines;
	  begin
		List.iter self#_render_line lines;
	  end;
	  GlDraw.ends();

	  let polyLines = def#polyLines() in
	  List.iter self#_renderPolyLine polyLines;




	  let x = _f#letterSpacing /. 4.0 in
	  let w = def#width +. x in
	  GlMat.translate () ~x:w;
	   *)

	method triangle points =

	  let v (p:point2) =
		let x = p#x and
			y = p#y in
		GlDraw.vertex2 (x,y)
	  in


	  GlDraw.begins `triangles;
	  List.iter v points;
	  GlDraw.ends()


	method lineloop (points:point2 list) = ()

	method render_string ?(mw=1000.0) (s:string) =
	  GlMat.push();
	  GlDraw.color (1.0,1.0,1.0);
	  let sl = String.length s in
	  let rec draw_inner index cw =
		if (cw > mw) || (index = sl) then
		  ()
		else
		  let char = s.[index] in
		  let code = int_of_char char in
		  let (glyph : glyph) = _f#getGlyph code in
		  let nw = glyph#width +. cw +. (_f#letterSpacing /. 4.0) in
		  if nw < mw then
			begin
			  self#_renderGlyph glyph;
			  let x = _f#letterSpacing /. 4.0 in
			  let w = glyph#width +. x in
			  GlMat.translate () ~x:w;
			  draw_inner (index+1) nw
			end
	  in
	  draw_inner 0 0.0;
	  GlMat.pop()


	method string_width s =
	  let result = ref 0.0 in
	  let add c =
		let code = (int_of_char c) in
		let glyph = _f#getGlyph code in
		let x = _f#letterSpacing /. 4.0 in
		let w = glyph#width +. x in
		result := !result +. (2.0 *. w) in
	  String.iter add s;
	  !result


  end
