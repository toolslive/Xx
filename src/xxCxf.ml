exception ParseException

class cxfFontReader = object(self)

  method read input_channel =

    let result = new XxFont.twlFont in
    let currentGlyph = ref (new XxChar.char_definition)in
    try
      while true do
        begin
          let line = input_line input_channel in
          if String.length line > 0 then
            let c0 = String.get line 0 in
            match c0 with
            | '#' -> ()
            | '[' ->
              begin
                self#_closeGlyph currentGlyph result;
                let hex = String.sub line 2 4 in
                let code = self#_code hex in
                currentGlyph := new XxChar.char_definition;
                result#setGlyph code !currentGlyph;
              end
            | 'L' -> self#_addLine line currentGlyph
            | _-> ()
        end
      done;
      result
    with End_of_file ->
      self#_closeGlyph currentGlyph result;
      result


  method private _closeGlyph currentGlyph result =
    !currentGlyph#finalize ()


 method private _code hex4 =
   let digits = "0123456789ABCDEF" in
   let int_of_hex_digit d =
     let rec lookup d i=
       if d = digits.[i] then
         i
       else
         lookup d (i+1)
     in lookup d 0
   in
   int_of_hex_digit hex4.[0] *  4096 +
     int_of_hex_digit hex4.[1] * 256 +
     int_of_hex_digit hex4.[2] * 16  +
     int_of_hex_digit hex4.[3]

  method private _addLine line currentGlyph =
    let lineDef = Str.string_after line 2 in
    let splitted = Str.split (Str.regexp ",") lineDef in
    let floats = List.map float_of_string splitted in
    let ins = new XxInstruction.line floats in
    !currentGlyph#addLine ins


  method dump (stream:out_channel) = ()

end



let create path =
  let channel = open_in path in
  let p = new cxfFontReader in
  let font = p#read channel in
  close_in channel;
  font
