[<RequireQualifiedAccess>]
module MicroFun.Bytes

open System
open System.Text
open System.Linq
open FsToolkit.ErrorHandling

let inline toBase64String (bytes: byte []) = Convert.ToBase64String(bytes)
let inline ofBase64String (s: string) = Convert.FromBase64String(s)

let inline tryOfBase64String s =
    Result.tryWith (fun () -> ofBase64String s)


let private indexToLowerHex =
    let create () =
        [| for i = 0 to 255 do
               yield i.ToString("x2") |]

    lazy (create ())

let private indexToUpperHex =
    let create () =
        [| for i = 0 to 255 do
               yield i.ToString("X2") |]

    lazy (create ())

let private hexToIndex =
    let create () =
        [| for i = 0 to 255 do
               indexToLowerHex.Value.[i], byte i |]
            .ToDictionary((fun (hex, byte) -> hex), (fun (hex, byte) -> byte), StringComparer.OrdinalIgnoreCase)

    lazy (create ())

// TODO: Test for performance against other implementations
let private toHexStringWith (cache: Lazy<string []>) (bytes: byte []) =
    let byteCount = bytes.Length

    if byteCount = 0 then
        ""
    else
        let data = cache.Value
        let charCount = byteCount <<< 1
        stringBuilderWith charCount {
            for index = 0 to byteCount - 1 do
                yield data.[int bytes.[index]]
        }
        //let sb = StringBuilder.withCapacity (byteCount <<< 1)

        //for index = 0 to byteCount - 1 do
        //    let hex = data.[int bytes.[index]]
        //    sb.Append(hex) |> ignore

        //sb.ToString()

let toUppercaseHexString = toHexStringWith indexToUpperHex
let toLowercaseHexString = toHexStringWith indexToLowerHex

[<RequireQualifiedAccess>]
type OfHexStringError =
    | ExpectedEvenChars of found: int
    | InvalidHexPair of found: string * atCharIndex: int

exception OfHexStringException of OfHexStringError

let tryOfHexString (s: string) : Result<byte[], OfHexStringError> =
    result {
        let stringLength = s.Length

        if stringLength = 0 then
            return [||]

        elif stringLength &&& 1 <> 0 then
            return! Error(OfHexStringError.ExpectedEvenChars stringLength)

        else
            let byteCount = s.Length >>> 1
            let data = hexToIndex.Value
            let bytes = Array.zeroCreate<byte> byteCount

            let rec loop byteIndex =
                result {
                    if byteIndex >= byteCount then
                        return ()
                    else
                        let charIndex = byteIndex <<< 1
                        let hex = s.Substring(charIndex, 2)
                        match data.TryGetValue(hex) with
                        | false, _ ->
                            return! Error(OfHexStringError.InvalidHexPair(hex, charIndex))
                        | true, byte ->
                            bytes.[byteIndex] <- byte
                            return! loop (byteIndex + 1)
                }

            do! loop 0

            return bytes
    }

let ofHexString (s: string) : byte[] =
    match tryOfHexString s with
    | Ok bytes -> bytes
    | Error error ->
        match error with
        | OfHexStringError.ExpectedEvenChars found ->
            failwith $"lang:MicroFun.Error.Bytes.ExpectedEvenChars|found={found}"
        | OfHexStringError.InvalidHexPair (found, atCharIndex) ->
            failwith $"lang:MicroFun.Error.Bytes.InvalidHexPair|found={found}|at={atCharIndex}"
