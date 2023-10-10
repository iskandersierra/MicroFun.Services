namespace MicroFun

open System

[<RequireQualifiedAccess>]
module Disposable =
    let inline dispose (x: #IDisposable) =
        if obj.ReferenceEquals(x, null) |> not then
            x.Dispose()

    let inline disposeObj (x: obj) =
        match x with
        | :? IDisposable as x -> x.Dispose()
        | _ -> ()

    let defer f =
        { new IDisposable with
            member __.Dispose() = f() }

[<AutoOpen>]
module DisposablePreamble =
    let inline dispose x = Disposable.dispose x
    let inline disposeObj x = Disposable.disposeObj x
    let inline defer f = Disposable.defer f
