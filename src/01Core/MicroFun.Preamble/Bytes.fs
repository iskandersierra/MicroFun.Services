[<RequireQualifiedAccess>]
module MicroFun.Bytes

open System
open System.Text
open System.Linq

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

let ofHexString (s: string) =
    let stringLength = s.Length

    if stringLength = 0 then
        [||]
    elif stringLength &&& 1 <> 0 then
        failwith $"lang:MicroFun.Error.Bytes.ExpectedEvenChars|found={stringLength}"
    else
        let byteCount = s.Length >>> 1
        let data = hexToIndex.Value
        let bytes = Array.init byteCount (fun byteIndex ->
            let hex = s.Substring(byteIndex <<< 1, 2)
            match data.TryGetValue(hex) with
            | false, _ -> failwith  $"lang:MicroFun.Error.Bytes.InvalidHexPair|found={hex}"
            | true, byte -> byte)

        bytes

let inline tryOfHexString s =
    Result.tryWith (fun () -> ofHexString s)
