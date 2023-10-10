[<RequireQualifiedAccess>]
module MicroFun.Option

let inline tryWith ([<InlineIfLambda>] f: unit -> 'a) =
    try
        Some (f())
    with
    | _ -> None
