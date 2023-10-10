[<AutoOpen>]
module MicroFun.Preamble

open System

let inline konst x = fun _ -> x
let inline konst2 x = fun _ _ -> x
let inline konst3 x = fun _ _ _ -> x

let inline flip f = fun x y -> f y x

let inline curry f = fun x y -> f (x, y)
let inline curry3 f = fun x y z -> f (x, y, z)

let inline uncurry f = fun (x, y) -> f x y
let inline uncurry3 f = fun (x, y, z) -> f x y z

let inline tee f x = f x; x

let isNotNull x = not (isNull x)
