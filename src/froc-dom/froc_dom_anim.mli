type color
type point = float * float
type shape = Dom.canvasRenderingContext2D -> unit

val color : ?a:int -> int -> int -> int -> color

val disk : point -> float -> color -> shape

val attach : Dom.canvas -> shape list Froc.behavior -> unit
