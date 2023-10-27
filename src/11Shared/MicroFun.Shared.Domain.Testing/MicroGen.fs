[<RequireQualifiedAccess>]
module MicroFun.Shared.Domain.Testing.MicroGen

open FsCheck
open MicroFun
open MicroFun.Shared.Domain
open Validus


[<RequireQualifiedAccess>]
module Result =
    let filterOk (source: Gen<Result<'a, 'b>>) =
        source
        |> Gen.filter (function
            | Ok _ -> true
            | Error _ -> false)
        |> Gen.map (function
            | Ok value -> value
            | Error _ -> unreachable ())

    let filterError (source: Gen<Result<'a, 'b>>) =
        source
        |> Gen.filter (function
            | Ok _ -> false
            | Error _ -> true)
        |> Gen.map (function
            | Error value -> value
            | Ok _ -> unreachable ())


[<RequireQualifiedAccess>]
module Record2 =
    let generate (g: {| field1: Gen<'a>; field2: Gen<'b> |}) mapper =
        gen {
            let! value1 = g.field1
            let! value2 = g.field2
            return mapper value1 value2
        }


[<RequireQualifiedAccess>]
module Record3 =
    let generate
        (g: {| field1: Gen<'a>
               field2: Gen<'b>
               field3: Gen<'c> |})
        mapper
        =
        gen {
            let! value1 = g.field1
            let! value2 = g.field2
            let! value3 = g.field3
            return mapper value1 value2 value3
        }


[<RequireQualifiedAccess>]
module Record4 =
    let generate
        (g: {| field1: Gen<'a>
               field2: Gen<'b>
               field3: Gen<'c>
               field4: Gen<'d> |})
        mapper
        =
        gen {
            let! value1 = g.field1
            let! value2 = g.field2
            let! value3 = g.field3
            let! value4 = g.field4
            return mapper value1 value2 value3 value4
        }


[<RequireQualifiedAccess>]
module Record5 =
    let generate
        (g: {| field1: Gen<'a>
               field2: Gen<'b>
               field3: Gen<'c>
               field4: Gen<'d>
               field5: Gen<'e> |})
        mapper
        =
        gen {
            let! value1 = g.field1
            let! value2 = g.field2
            let! value3 = g.field3
            let! value4 = g.field4
            let! value5 = g.field5
            return mapper value1 value2 value3 value4 value5
        }


[<RequireQualifiedAccess>]
module ValueType =
    let tryMap
        (tryParse: 'underlying -> ValidationResult<'valueType>)
        (underlyingGenerator: Gen<'underlying>)
        : Gen<'valueType> =
        underlyingGenerator
        |> Gen.map tryParse
        |> Result.filterOk

    let generate (valueType: IValueType<'valueType, 'underlying>) = tryMap valueType.TryParse


[<RequireQualifiedAccess>]
module Word =
    let generateWithOptions
        (options: {| firstLetter: Gen<char>
                     letter: Gen<char>
                     length: Gen<int> |})
        =
        gen {
            let! length = options.length
            let length = length |> max 1
            let! firstLetter = options.firstLetter

            if length = 1 then
                return firstLetter |> string
            else
                let! letters = Gen.listOfLength (length - 1) options.letter
                return firstLetter :: letters |> List.toArray |> string
        }

    let generateFirstWordRaw =
        generateWithOptions
            {| firstLetter = Gen.elements [ 'A' .. 'Z' ]
               letter = Gen.elements [ 'a' .. 'z' ]
               length = Gen.choose (1, 10) |}

    let generateFirstWord =
        generateFirstWordRaw |> Gen.map FirstWord

    let generateWordRaw =
        generateWithOptions
            {| firstLetter = Gen.elements [ 'a' .. 'z' ]
               letter = Gen.elements [ 'a' .. 'z' ]
               length = Gen.choose (1, 10) |}

    let generateWord = generateWordRaw |> Gen.map Word


[<RequireQualifiedAccess>]
module Sentence =
    let generateWithOptions
        (options: {| firstWord: Gen<FirstWord>
                     word: Gen<Word>
                     length: Gen<int> |})
        =
        gen {
            let! length = options.length
            let length = length |> max 1
            let! firstWord = options.firstWord

            if length = 1 then
                return firstWord.Get
            else
                let! words = Gen.listOfLength (length - 1) options.word

                return
                    firstWord.Get :: (words |> List.map Word.getValue)
                    |> String.concat " "
        }

    let generateSentenceRaw =
        generateWithOptions
            {| firstWord = Word.generateFirstWord
               word = Word.generateWord
               length = Gen.choose (3, 10) |}

    let generateSentence = generateSentenceRaw |> Gen.map Sentence


[<RequireQualifiedAccess>]
module Paragraph =
    let generateWithOptions
        (options: {| sentence: Gen<Sentence>
                     length: Gen<int> |})
        =
        gen {
            let! length = options.length
            let length = length |> max 0
            let! sentences = Gen.listOfLength length options.sentence

            return
                sentences
                |> List.map Sentence.getValue
                |> String.concat ". "
        }

    let generateParagraphRaw =
        generateWithOptions
            {| sentence = Sentence.generateSentence
               length = Gen.choose (1, 10) |}

    let generateParagraph =
        generateParagraphRaw |> Gen.map Paragraph
