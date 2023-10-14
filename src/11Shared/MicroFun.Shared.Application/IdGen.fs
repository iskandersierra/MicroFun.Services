namespace MicroFun.Shared.Application

open System
open MicroFun

type IIdGen =
    abstract member NewId : unit -> string

type GuidIdGen() =
    interface IIdGen with
        member this.NewId() = Guid.NewGuid().ToString("N")

type CryptoRandomHexIdGen(?byteCount: int) =
    let byteCount = defaultArg byteCount 16

    interface IIdGen with
        member this.NewId() =
            Crypto.Random.getBytes byteCount
            |> Bytes.toLowercaseHexString

type PrefixedIdGen(prefix: string, innerGenerator: IIdGen) =
    interface IIdGen with
        member this.NewId() =
            let innerId = innerGenerator.NewId()
            $"{prefix}{innerId}"
