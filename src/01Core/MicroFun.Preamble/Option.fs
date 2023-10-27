[<RequireQualifiedAccess>]
module MicroFun.Option

let inline ofValueOption source =
    match source with
    | ValueSome x -> Some x
    | ValueNone -> None

let inline toValueOption source =
    match source with
    | Some x -> ValueSome x
    | None -> ValueNone

let inline ofResult source =
    match source with
    | Ok x -> Some x
    | Error _ -> None

let inline toResult source =
    match source with
    | Some x -> Ok x
    | None -> Error ()


let inline tryWith ([<InlineIfLambda>] f: unit -> 'a) =
    try
        Some (f())
    with
    | _ -> None
