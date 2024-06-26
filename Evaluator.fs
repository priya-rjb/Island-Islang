module Evaluator 
open AST

type Env = Map<string, Dims * string>

// let scalingFunction(outer_scale: Dims, inner_scale: Dims): ? =

let evalName(name: string, outer_scale: Dims)(env: Map<string, Dims * string>): string =
    let mod_name = ("Name \"" + name + "\"")
    if Map.containsKey mod_name env then
        let inner_scale, svg = env[mod_name]
        // TODO: CHANGE THE SCALE HERE
        svg
        else
            printfn "Undefined variable."
            exit 1

let evalCircle (point,radius, scale: Dims):string =
    let mininimum = min scale.w scale.h
    let width = scale.w/2
    let height = scale.h/2
    "   <circle cx =\"" +  (((point.x)+width+height) |> string) + "\"" +
    " cy =\"" +  ((point.y+width+height) |> string) + "\"" +
    " r =\"" + ((radius*(mininimum/5)) |> string) + "\"" +
    " stroke= \"black\" stroke-width=\"4\" fill =\"" + canvas_color + "\"/>\n"


let rec evalComponents
    (components: Component list)(scale: Dims)
    (env: Map<string, Dims * string>)
    : string =
    match components with
    | [] -> ""
    | x::xs -> 
        let eval1 = match x with
                    | Name (name) -> evalName (name, scale) env 
                    | Circle (point, radius) -> evalCircle(point, radius, scale)
        let eval2 = evalComponents xs scale env
        eval1 + eval2


let evalDefinition
    (def: Definition)(env: Map<string, Dims * string>)
    : string * Map<string, Dims * string> =

    match def.name, def.dims, def.components with
    | _,_,[] -> ("",env)
    | n,x,_ ->
        let svg_str = evalComponents def.components x env
        let env1 = env.Add(n, (x , svg_str))
        svg_str, env1


let rec evalCanvas(canvas: Canvas)(env: Map<string, Dims * string>): string = 
    match canvas with
    | Canvas [] -> ""
    | Canvas [x] ->
        let eval, env1 = evalDefinition (x)(env)
        eval
    | Canvas (x::xs)->
        let _, env1 = evalDefinition (x) (env)
        evalCanvas(Canvas xs)(env1)


let eval (canvas: Canvas): string  = 
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\" style=\"background-color:" + canvas_color+"\">" + "\n" +
    (evalCanvas canvas Map.empty)
    + "</svg>\n"
