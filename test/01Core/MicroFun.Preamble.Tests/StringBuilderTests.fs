module StringBuilderTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open MicroFun

[<Literal>]
let Verbose = false

[<Fact>]
let ``stringBuilder.zero`` () =
    let actual = stringBuilder { () }
    let expected = ""
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield string`` () =
    let actual = stringBuilder { yield "value" }
    let expected = "value"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield char[]`` () =
    let actual =
        stringBuilder { yield [| 'v'; 'a'; 'l'; 'u'; 'e' |] }

    let expected = "value"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield char`` () =
    let actual = stringBuilder { yield 'v' }
    let expected = "v"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield StringBuilder`` () =
    let other = System.Text.StringBuilder("init")
    let actual = stringBuilder { yield other }
    let expected = "init"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield obj`` () =
    let actual =
        stringBuilder { yield {| hello = "world" |} :> obj }

    let expected = @"{ hello = ""world"" }"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield bool`` () =
    let actual = stringBuilder { yield true }
    let expected = "True"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield uint8`` () =
    let actual = stringBuilder { yield uint8 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield uint16`` () =
    let actual = stringBuilder { yield uint16 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield uint32`` () =
    let actual = stringBuilder { yield uint32 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield uint64`` () =
    let actual = stringBuilder { yield uint64 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield int8`` () =
    let actual = stringBuilder { yield int8 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield int16`` () =
    let actual = stringBuilder { yield int16 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield int32`` () =
    let actual = stringBuilder { yield int32 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield int64`` () =
    let actual = stringBuilder { yield int64 42 }
    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield decimal`` () =
    let actual = stringBuilder { yield decimal 3.14 }
    let expected = "3.14"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield float32`` () =
    let actual = stringBuilder { yield float32 3.14 }
    let expected = "3.14"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.yield float`` () =
    let actual = stringBuilder { yield float 3.14 }
    let expected = "3.14"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.combine`` () =
    let actual =
        stringBuilder {
            yield 3
            yield '.'
            yield "14"
        }

    let expected = "3.14"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.tryWith no error`` () =
    let actual =
        stringBuilder {
            try
                yield 42
            with
            | _ -> yield "err"
        }

    let expected = "42"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.tryWith error`` () =
    let actual =
        stringBuilder {
            try
                yield 42
                failwith "err"
            with
            | exn -> yield exn.Message
        }

    let expected = "42err"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.tryFinally`` () =
    let mutable finallized = false
    let actual =
        stringBuilder {
            try
                yield 42
            finally
                finallized <- true
        }

    let expected = "42"
    test <@ actual = expected @>
    test <@ finallized @>

[<Fact>]
let ``stringBuilder.using`` () =
    let mutable disposed = false
    let actual =
        stringBuilder {
            use _ = defer (fun () -> disposed <- true)
            yield 42
        }

    let expected = "42"
    test <@ actual = expected @>
    test <@ disposed @>

[<Fact>]
let ``stringBuilder.whileLoop`` () =
    let mutable index = 0
    let actual =
        stringBuilder {
            while index < 10 do
                yield index
                index <- index + 1
        }

    let expected = "0123456789"
    test <@ actual = expected @>
    test <@ index = 10 @>

[<Fact>]
let ``stringBuilder.forToLoop`` () =
    let actual =
        stringBuilder {
            for index = 0 to 9 do
                yield index
        }

    let expected = "0123456789"
    test <@ actual = expected @>

[<Fact>]
let ``stringBuilder.forInLoop`` () =
    let actual =
        stringBuilder {
            let range = seq { 0 .. 9 }
            for index in range do
                yield index
        }

    let expected = "0123456789"
    test <@ actual = expected @>
