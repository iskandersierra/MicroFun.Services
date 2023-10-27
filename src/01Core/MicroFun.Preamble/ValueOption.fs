[<RequireQualifiedAccess>]
module MicroFun.ValueOption

let inline ofOption (source: 'a option) =
    match source with
    | Some x -> ValueSome x
    | None -> ValueNone

let inline toOption (source: 'a voption) =
    match source with
    | ValueSome x -> Some x
    | ValueNone -> None

let inline tryWith ([<InlineIfLambda>] f: unit -> 'a) =
    try
        ValueSome (f())
    with
    | _ -> ValueNone
