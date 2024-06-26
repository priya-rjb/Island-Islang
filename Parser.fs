module Parser

open Combinator
open AST

let pdefn,pdefnImpl = recparser()

(* my_ws
 *   Consider any non-newline whitespace or
 *   a comment to be whitespace
 *)
let my_ws = (pwsNoNL0 |>> (fun _ -> true))

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween my_ws p my_ws

(* pnum
 *   Parses a number.
 *)
let pnum: Parser<int> = pmany1 pdigit
                         |>> (fun ds ->
                                int (stringify ds)
                             ) <!> "pnum"

let ptwonums: Parser<int*int> = pseq
                                    (pad pnum)
                                    (pright (pchar ',') (pad pnum))
                                    (fun (x,y) -> (x,y)) <!> "ptwonums"

let ppoint: Parser<Point> = pright 
                                (pstr "point=")
                                (pbetween (pchar '(') ptwonums (pchar ')'))
                                |>> (fun (xval,yval) -> {x=xval; y=yval})
                                <!> "ppoint"

let pradius: Parser<int> = pright (pstr "radius=") (pad pnum) <!> "pradius"

let pcircle: Parser<Component> = pright 
                                    (pstr "circle")
                                    (pseq
                                        (pad ppoint)
                                        (pad pradius)
                                        (fun (x,y) ->
                                            Circle(x,y))
                                    ) <!> "pcircle"

(* pname
 *   Parses a variable.  Variable names are at least one
 *   character long, starting with a letter, followed by
 *   any combination of letters or numbers.
 *)
let pnamechar: Parser<char> = pletter <|> pdigit <!> "pnamechar"

let pname: Parser<Component> = pseq
                                pletter 
                                (pmany0 pnamechar |>> stringify)
                                (fun (c: char, s: string) -> (string c) + s)
                            |>> (fun v -> Name v) <!> "pname"

let pcomponent: Parser<Component> = pright pws1 (pcircle <|> pname) <!> "pcomponent"

let pcomponents: Parser<Component list> = pmany1
                                            (pleft pcomponent pnl)
                                            <!> "pcomponents"

let pdims: Parser<Dims> = pseq
                            pnum
                            (pbetween (pchar 'x') pnum (pleft (pstr " is:") pnl))
                            (fun (x,y) -> {w=x; h=y})
                        <!> "pdims"

(* pexpr
 *   Parses an arbitrary expression.  In general, tries
 *   to parse the most distinguisable/most complex thing
 *   first.
 *)
pdefnImpl := pseq
                (pseq pname (pright pws1 pdims) (fun (x,y) -> (string x,y))) 
                (pcomponents) 
                (fun ((x,y),z) -> {name=x; dims=y; components=z})
                <!> "pdefn"

(* pdefns
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pdefns = pmany1 (pleft pdefn pws0 ) |>> Canvas <!> "pdefns"

(* grammar
 *  Top level parser definition.  Call this one
 *  if you want a Blub parser.
 *)
let canvas = pleft pdefns peof <!> "canvas"

(* parse
 *  User-friendly function that calls the Blub parser
 *  and returns an optional Expr.
 *)
let parse (input: string)(do_debug: bool) : Canvas option =
    let i = (if do_debug then debug else prepare) input
    match canvas i with
    | Success(ast,_) -> Some ast
    | Failure(_,_)   -> None
