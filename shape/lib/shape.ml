type shape =
  | Circle of float
  | Rectangle of float * float

let perimeter = function
  | Circle r -> Circle.perimeter r
  | Rectangle (w, h) -> w * h

let area = function
  | Circle r -> Circle.area r
  | Rectangle (w, h) -> Rectangle.area w h
