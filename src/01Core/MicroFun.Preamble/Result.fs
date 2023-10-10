[<RequireQualifiedAccess>]
module MicroFun.Result

let inline tryWith ([<InlineIfLambda>] f: unit -> 'a) =
    try
        Ok (f())
    with
    | exn -> Error exn
