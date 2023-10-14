namespace MicroFun.Shared.Application

open System

type IClock =
    abstract member UtcNow : unit -> DateTime


type SystemClock() =
    interface IClock with
        member this.UtcNow() = DateTime.UtcNow

type RequestClock = RequestClock of clock: IClock

[<RequireQualifiedAccess>]
module RequestClock =
    let create (liveClock: IClock) =
        let requestStart = liveClock.UtcNow()

        { new IClock with
            member this.UtcNow() = requestStart }
