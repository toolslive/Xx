class colour (c:float*float*float*float) = object(self)
  val _c = c
  method get_rgba = _c
end
