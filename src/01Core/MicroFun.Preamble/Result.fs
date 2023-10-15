[<RequireQualifiedAccess>]
module MicroFun.Result

let inline tryWith ([<InlineIfLambda>] f: unit -> 'a) =
    try
        Ok (f())
    with
    | exn -> Error exn

let inline either ([<InlineIfLambda>] whenOk: 'a -> 'r) ([<InlineIfLambda>] whenError: 'e -> 'r) =
    function
    | Ok value -> whenOk value
    | Error error -> whenError error

let inline getOr ([<InlineIfLambda>] whenError: 'e -> 'a) =
    function
    | Ok value -> value
    | Error error -> whenError error
