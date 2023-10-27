module MicroFun.Preamble.Tests.BytesTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open MicroFun

[<Literal>]
let Verbose = false


[<Property(Verbose = Verbose)>]
let ``Bytes.toUppercaseHexString`` (NonNull bytes) =
    let actual = Bytes.toUppercaseHexString bytes
    let expected = BitConverter.ToString(bytes).ToUpperInvariant().Replace("-", "")
    test <@ actual = expected @>

[<Property(Verbose = Verbose)>]
let ``Bytes.toLowercaseHexString`` (NonNull bytes) =
    let actual = Bytes.toLowercaseHexString bytes
    let expected = BitConverter.ToString(bytes).ToLowerInvariant().Replace("-", "")
    test <@ actual = expected @>

[<Property(Verbose = Verbose)>]
let ``Bytes.ofHexString with odd number of characters`` (NonNegativeInt halfLength) =
    let length = (halfLength <<< 1) + 1
    let text = String.init length (konst "0")
    let expected = $@"lang:MicroFun.Error.Bytes.ExpectedEvenChars|found={length}"
    let exn = Assert.Throws(fun () -> Bytes.ofHexString text |> ignore)
    Assert.Equal<string>(expected, exn.Message)

[<Theory>]
[<InlineData("0G", "0G", 0)>]
[<InlineData("AbCDeFgh00", "gh", 6)>]
let ``Bytes.ofHexString with invalid hex pairs`` (text: string) (invalid: string) (atIndex: int) =
    let expected = $@"lang:MicroFun.Error.Bytes.InvalidHexPair|found={invalid}|at={atIndex}"
    let exn = Assert.Throws(fun () -> Bytes.ofHexString text |> ignore)
    Assert.Equal<string>(expected, exn.Message)


let ``Bytes.ofHexString data`` () =
    let data (text: string) (expected: byte[]) = [| box text; box expected |]
    seq {
        data "" [||]
        data "00" [| 0uy |]
        data "C0ffEe" [| 0xC0uy; 0xFFuy; 0xEEuy |]
    }

[<Theory>]
[<MemberData(nameof ``Bytes.ofHexString data``)>]
let ``Bytes.ofHexString`` (text: string) (expected: byte[]) =
    let actual = Bytes.ofHexString text
    Assert.Equal<byte>(expected, actual)
