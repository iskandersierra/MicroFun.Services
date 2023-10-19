namespace MicroFun

open System
open System.Diagnostics
open System.Runtime.ExceptionServices

[<RequireQualifiedAccess>]
module Exn =
    let inline message (exn: exn) = exn.Message

    let notImplemented () = NotImplementedException()

    let unreachable msg = UnreachableException(message = msg)

    let capture (exn: exn) = ExceptionDispatchInfo.Capture(exn)

    let reraise (exn: exn) =
        (capture exn).Throw()
        raise <| unreachable "reraise"

    let rec toSeq (exn: exn) =
        seq {
            if exn <> null then
                yield exn

                match exn with
                | :? AggregateException as aggregate -> yield! aggregate.InnerExceptions |> Seq.collect toSeq
                | _ -> yield! toSeq exn.InnerException
        }


[<AutoOpen>]
module ExnPreamble =
    let inline notImpl _ = raise <| NotImplementedException()

    let inline unreachable _ = raise <| UnreachableException()
