[<RequireQualifiedAccess>]
module MicroFun.Shared.Domain.Testing.MicroShrink

open Xunit
open FsCheck
open FsCheck.Xunit
open MicroFun
open MicroFun.Shared.Domain
open Validus


[<RequireQualifiedAccess>]
module Result =
    let filterOk (source: Result<'a, 'b> seq) =
        source
        |> Seq.collect (function
            | Ok value -> [ value ]
            | Error _ -> [])

    let filterError (source: Result<'a, 'b> seq) =
        source
        |> Seq.collect (function
            | Ok _ -> []
            | Error value -> [ value ])


[<Struct>]
type private EnumeratorStatus<'a> = { value: 'a; hasMore: bool }

let private getEnumeratorStatus previousValue =
    Seq.tryMoveNext
    >> function
        | None ->
            { value = previousValue
              hasMore = false }
        | Some value -> { value = value; hasMore = true }


[<RequireQualifiedAccess>]
module Record2 =
    let shrink
        (s: {| shrinker1: 'a -> 'a seq
               shrinker2: 'b -> 'b seq |})
        (mapper: 'a -> 'b -> 'record)
        (unmapper: 'record -> 'a * 'b)
        (record: 'record)
        =
        seq {
            let value1, value2 = unmapper record

            use enumerator1 =
                s.shrinker1 value1 |> Seq.getEnumerator

            use enumerator2 =
                s.shrinker2 value2 |> Seq.getEnumerator

            let rec loop index (status1: EnumeratorStatus<'a>) (status2: EnumeratorStatus<'b>) =
                seq {
                    match index with
                    | 0 ->
                        if status1.hasMore then
                            yield mapper status1.value status2.value
                            yield! loop 1 (getEnumeratorStatus status1.value enumerator1) status2
                        else
                            yield! loop 1 status1 status2

                    | 1 ->
                        if status2.hasMore then
                            yield mapper status1.value status2.value
                            yield! loop 2 status1 (getEnumeratorStatus status2.value enumerator2)
                        else
                            yield! loop 2 status1 status2

                    | _ ->
                        match status1.hasMore, status2.hasMore with
                        | false, false -> ()
                        | _ -> yield! loop 0 status1 status2
                }

            yield! loop 0 (getEnumeratorStatus value1 enumerator1) (getEnumeratorStatus value2 enumerator2)
        }


[<RequireQualifiedAccess>]
module Record3 =
    let shrink
        (s: {| shrinker1: 'a -> 'a seq
               shrinker2: 'b -> 'b seq
               shrinker3: 'c -> 'c seq |})
        (mapper: 'a -> 'b -> 'c -> 'record)
        (unmapper: 'record -> 'a * 'b * 'c)
        (record: 'record)
        =
        seq {
            let value1, value2, value3 = unmapper record

            use enumerator1 =
                s.shrinker1 value1 |> Seq.getEnumerator

            use enumerator2 =
                s.shrinker2 value2 |> Seq.getEnumerator

            use enumerator3 =
                s.shrinker3 value3 |> Seq.getEnumerator

            let rec loop
                index
                (status1: EnumeratorStatus<'a>)
                (status2: EnumeratorStatus<'b>)
                (status3: EnumeratorStatus<'c>)
                =
                seq {
                    match index with
                    | 0 ->
                        if status1.hasMore then
                            yield mapper status1.value status2.value status3.value
                            yield! loop 1 (getEnumeratorStatus status1.value enumerator1) status2 status3
                        else
                            yield! loop 1 status1 status2 status3

                    | 1 ->
                        if status2.hasMore then
                            yield mapper status1.value status2.value status3.value
                            yield! loop 2 status1 (getEnumeratorStatus status2.value enumerator2) status3
                        else
                            yield! loop 2 status1 status2 status3

                    | 2 ->
                        if status3.hasMore then
                            yield mapper status1.value status2.value status3.value
                            yield! loop 3 status1 status2 (getEnumeratorStatus status3.value enumerator3)
                        else
                            yield! loop 3 status1 status2 status3

                    | _ ->
                        match status1.hasMore, status2.hasMore, status3.hasMore with
                        | false, false, false -> ()
                        | _ -> yield! loop 0 status1 status2 status3
                }

            yield!
                loop
                    0
                    (getEnumeratorStatus value1 enumerator1)
                    (getEnumeratorStatus value2 enumerator2)
                    (getEnumeratorStatus value3 enumerator3)
        }


[<RequireQualifiedAccess>]
module Record4 =
    let shrink
        (s: {| shrinker1: 'a -> 'a seq
               shrinker2: 'b -> 'b seq
               shrinker3: 'c -> 'c seq
               shrinker4: 'd -> 'd seq |})
        (mapper: 'a -> 'b -> 'c -> 'd -> 'record)
        (unmapper: 'record -> 'a * 'b * 'c * 'd)
        (record: 'record)
        =
        seq {
            let value1, value2, value3, value4 = unmapper record

            use enumerator1 =
                s.shrinker1 value1 |> Seq.getEnumerator

            use enumerator2 =
                s.shrinker2 value2 |> Seq.getEnumerator

            use enumerator3 =
                s.shrinker3 value3 |> Seq.getEnumerator

            use enumerator4 =
                s.shrinker4 value4 |> Seq.getEnumerator

            let rec loop
                index
                (status1: EnumeratorStatus<'a>)
                (status2: EnumeratorStatus<'b>)
                (status3: EnumeratorStatus<'c>)
                (status4: EnumeratorStatus<'d>)
                =
                seq {
                    match index with
                    | 0 ->
                        if status1.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value
                            yield! loop 1 (getEnumeratorStatus status1.value enumerator1) status2 status3 status4
                        else
                            yield! loop 1 status1 status2 status3 status4

                    | 1 ->
                        if status2.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value
                            yield! loop 2 status1 (getEnumeratorStatus status2.value enumerator2) status3 status4
                        else
                            yield! loop 2 status1 status2 status3 status4

                    | 2 ->
                        if status3.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value
                            yield! loop 3 status1 status2 (getEnumeratorStatus status3.value enumerator3) status4
                        else
                            yield! loop 3 status1 status2 status3 status4

                    | 3 ->
                        if status4.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value
                            yield! loop 4 status1 status2 status3 (getEnumeratorStatus status4.value enumerator4)
                        else
                            yield! loop 4 status1 status2 status3 status4

                    | _ ->
                        match status1.hasMore, status2.hasMore, status3.hasMore, status4.hasMore with
                        | false, false, false, false -> ()
                        | _ -> yield! loop 0 status1 status2 status3 status4
                }

            yield!
                loop
                    0
                    (getEnumeratorStatus value1 enumerator1)
                    (getEnumeratorStatus value2 enumerator2)
                    (getEnumeratorStatus value3 enumerator3)
                    (getEnumeratorStatus value4 enumerator4)
        }


[<RequireQualifiedAccess>]
module Record5 =
    let shrink
        (s: {| shrinker1: 'a -> 'a seq
               shrinker2: 'b -> 'b seq
               shrinker3: 'c -> 'c seq
               shrinker4: 'd -> 'd seq
               shrinker5: 'e -> 'e seq |})
        (mapper: 'a -> 'b -> 'c -> 'd -> 'e -> 'record)
        (unmapper: 'record -> 'a * 'b * 'c * 'd * 'e)
        (record: 'record)
        =
        seq {
            let value1, value2, value3, value4, value5 = unmapper record

            use enumerator1 =
                s.shrinker1 value1 |> Seq.getEnumerator

            use enumerator2 =
                s.shrinker2 value2 |> Seq.getEnumerator

            use enumerator3 =
                s.shrinker3 value3 |> Seq.getEnumerator

            use enumerator4 =
                s.shrinker4 value4 |> Seq.getEnumerator

            use enumerator5 =
                s.shrinker5 value5 |> Seq.getEnumerator

            let rec loop
                index
                (status1: EnumeratorStatus<'a>)
                (status2: EnumeratorStatus<'b>)
                (status3: EnumeratorStatus<'c>)
                (status4: EnumeratorStatus<'d>)
                (status5: EnumeratorStatus<'e>)
                =
                seq {
                    match index with
                    | 0 ->
                        if status1.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value status5.value
                            yield! loop 1 (getEnumeratorStatus status1.value enumerator1) status2 status3 status4 status5
                        else
                            yield! loop 1 status1 status2 status3 status4 status5

                    | 1 ->
                        if status2.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value status5.value
                            yield! loop 2 status1 (getEnumeratorStatus status2.value enumerator2) status3 status4 status5
                        else
                            yield! loop 2 status1 status2 status3 status4 status5

                    | 2 ->
                        if status3.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value status5.value
                            yield! loop 3 status1 status2 (getEnumeratorStatus status3.value enumerator3) status4 status5
                        else
                            yield! loop 3 status1 status2 status3 status4 status5

                    | 3 ->
                        if status4.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value status5.value
                            yield! loop 4 status1 status2 status3 (getEnumeratorStatus status4.value enumerator4) status5
                        else
                            yield! loop 4 status1 status2 status3 status4 status5

                    | 4 ->
                        if status5.hasMore then
                            yield mapper status1.value status2.value status3.value status4.value status5.value
                            yield! loop 5 status1 status2 status3 status4 (getEnumeratorStatus status5.value enumerator5)
                        else
                            yield! loop 5 status1 status2 status3 status4 status5

                    | _ ->
                        match status1.hasMore, status2.hasMore, status3.hasMore, status4.hasMore, status5.hasMore with
                        | false, false, false, false, false -> ()
                        | _ -> yield! loop 0 status1 status2 status3 status4 status5
                }

            yield!
                loop
                    0
                    (getEnumeratorStatus value1 enumerator1)
                    (getEnumeratorStatus value2 enumerator2)
                    (getEnumeratorStatus value3 enumerator3)
                    (getEnumeratorStatus value4 enumerator4)
                    (getEnumeratorStatus value5 enumerator5)
        }


[<RequireQualifiedAccess>]
module ValueType =
    let tryMap
        (getValue: 'valueType -> 'underlying)
        (tryParse: 'underlying -> ValidationResult<'valueType>)
        (underlyingShrinker: 'underlying -> 'underlying seq)
        : 'valueType -> 'valueType seq =
        getValue
        >> underlyingShrinker
        >> Seq.map tryParse
        >> Result.filterOk

    let shrink (valueType: IValueType<'valueType, 'underlying>) =
        tryMap valueType.GetValue valueType.TryParse


[<RequireQualifiedAccess>]
module Word =
    let shrinkRaw word =
        seq {
            let mutable length = String.length word

            while length >= 1 do
                yield word.Substring(0, length)
                length <- length >>> 1
        }

    let shrinkFirstWord (FirstWord word) = word |> shrinkRaw |> Seq.map FirstWord

    let shrinkWord (Word word) = word |> shrinkRaw |> Seq.map Word


[<RequireQualifiedAccess>]
module Sentence =
    let shrinkRaw sentence =
        seq {
            let words = sentence |> String.splitByChar ' '
            let mutable length = words.Length

            while length >= 1 do
                yield words |> Seq.take length |> String.concat " "
                length <- length >>> 1
        }

    let shrinkSentence (Sentence sentence) =
        sentence |> shrinkRaw |> Seq.map Sentence


[<RequireQualifiedAccess>]
module Paragraph =
    let shrinkRaw paragraph =
        seq {
            let sentences = paragraph |> String.splitBy ". "
            let mutable length = sentences.Length

            while length >= 1 do
                yield sentences |> Seq.take length |> String.concat ". "
                length <- length >>> 1
        }

    let shrinkParagraph (Paragraph paragraph) =
        paragraph |> shrinkRaw |> Seq.map Paragraph
