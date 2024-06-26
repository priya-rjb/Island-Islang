module AST

type Point = {x: int; y: int}

type Dims = {w: int; h: int}

type Component =
| Name of string
| Circle of Point * int

type Definition = {name: string; dims: Dims; components: Component list}

type Canvas = Canvas of Definition list

let CANVAS_SZ = 400
let canvas_color = "navajowhite"