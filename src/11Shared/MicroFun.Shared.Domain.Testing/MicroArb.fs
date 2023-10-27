[<RequireQualifiedAccess>]
module MicroFun.Shared.Domain.Testing.MicroArb

open FsCheck
open MicroFun.Shared.Domain
open Validus


[<RequireQualifiedAccess>]
module Record2 =
    let arbitrary
        (s: {| field1: Arbitrary<'a>
               field2: Arbitrary<'b> |})
        mapper
        unmapper
        : Arbitrary<'record>
        =
        let generator =
            MicroGen.Record2.generate
                {| field1 = s.field1.Generator
                   field2 = s.field2.Generator |}
                mapper

        let shrinker =
            MicroShrink.Record2.shrink
                {| shrinker1 = s.field1.Shrinker
                   shrinker2 = s.field2.Shrinker |}
                mapper
                unmapper

        Arb.fromGenShrink (generator, shrinker)


[<RequireQualifiedAccess>]
module Record3 =
    let arbitrary
        (s: {| field1: Arbitrary<'a>
               field2: Arbitrary<'b>
               field3: Arbitrary<'c> |})
        mapper
        unmapper
        : Arbitrary<'record>
        =
        let generator =
            MicroGen.Record3.generate
                {| field1 = s.field1.Generator
                   field2 = s.field2.Generator
                   field3 = s.field3.Generator |}
                mapper

        let shrinker =
            MicroShrink.Record3.shrink
                {| shrinker1 = s.field1.Shrinker
                   shrinker2 = s.field2.Shrinker
                   shrinker3 = s.field3.Shrinker |}
                mapper
                unmapper

        Arb.fromGenShrink (generator, shrinker)


[<RequireQualifiedAccess>]
module Record4 =
    let arbitrary
        (s: {| field1: Arbitrary<'a>
               field2: Arbitrary<'b>
               field3: Arbitrary<'c>
               field4: Arbitrary<'d> |})
        mapper
        unmapper
        : Arbitrary<'record>
        =
        let generator =
            MicroGen.Record4.generate
                {| field1 = s.field1.Generator
                   field2 = s.field2.Generator
                   field3 = s.field3.Generator
                   field4 = s.field4.Generator |}
                mapper

        let shrinker =
            MicroShrink.Record4.shrink
                {| shrinker1 = s.field1.Shrinker
                   shrinker2 = s.field2.Shrinker
                   shrinker3 = s.field3.Shrinker
                   shrinker4 = s.field4.Shrinker |}
                mapper
                unmapper

        Arb.fromGenShrink (generator, shrinker)


[<RequireQualifiedAccess>]
module Record5 =
    let arbitrary
        (s: {| field1: Arbitrary<'a>
               field2: Arbitrary<'b>
               field3: Arbitrary<'c>
               field4: Arbitrary<'d>
               field5: Arbitrary<'e> |})
        mapper
        unmapper
        : Arbitrary<'record>
        =
        let generator =
            MicroGen.Record5.generate
                {| field1 = s.field1.Generator
                   field2 = s.field2.Generator
                   field3 = s.field3.Generator
                   field4 = s.field4.Generator
                   field5 = s.field5.Generator |}
                mapper

        let shrinker =
            MicroShrink.Record5.shrink
                {| shrinker1 = s.field1.Shrinker
                   shrinker2 = s.field2.Shrinker
                   shrinker3 = s.field3.Shrinker
                   shrinker4 = s.field4.Shrinker
                   shrinker5 = s.field5.Shrinker |}
                mapper
                unmapper

        Arb.fromGenShrink (generator, shrinker)


[<RequireQualifiedAccess>]
module ValueType =
    let tryMap
        (getValue: 'valueType -> 'underlying)
        (tryParse: 'underlying -> ValidationResult<'valueType>)
        (underlyingGenerator: Gen<'underlying>)
        (underlyingShrinker: 'underlying -> 'underlying seq)
        : Arbitrary<'valueType> =
        let generator =
            MicroGen.ValueType.tryMap tryParse underlyingGenerator

        let shrinker =
            MicroShrink.ValueType.tryMap getValue tryParse underlyingShrinker

        Arb.fromGenShrink (generator, shrinker)

    let tryMapFromArb
        (getValue: 'valueType -> 'underlying)
        (tryParse: 'underlying -> ValidationResult<'valueType>)
        (underlyingArb: Arbitrary<'underlying>)
        : Arbitrary<'valueType> =
        tryMap getValue tryParse underlyingArb.Generator underlyingArb.Shrinker

    let arbitrary
        (valueType: IValueType<'valueType, 'underlying>)
        (underlyingGenerator: Gen<'underlying>)
        (underlyingShrinker: 'underlying -> 'underlying seq)
        : Arbitrary<'valueType> =
        tryMap valueType.GetValue valueType.TryParse underlyingGenerator underlyingShrinker

    let arbitraryFromArb (valueType: IValueType<'valueType, 'underlying>) (underlyingArb: Arbitrary<'underlying>) =
        tryMap valueType.GetValue valueType.TryParse underlyingArb.Generator underlyingArb.Shrinker


[<RequireQualifiedAccess>]
module Word =
    let withOptions options =
        let generator =
            MicroGen.Word.generateWithOptions options

        Arb.fromGenShrink (generator, MicroShrink.Word.shrinkRaw)

    let firstWord () =
        Arb.fromGenShrink (MicroGen.Word.generateFirstWord, MicroShrink.Word.shrinkFirstWord)

    let word () =
        Arb.fromGenShrink (MicroGen.Word.generateWord, MicroShrink.Word.shrinkWord)


[<RequireQualifiedAccess>]
module Sentence =
    let withOptions options =
        let generator =
            MicroGen.Sentence.generateWithOptions options

        Arb.fromGenShrink (generator, MicroShrink.Sentence.shrinkRaw)

    let sentence () =
        Arb.fromGenShrink (MicroGen.Sentence.generateSentence, MicroShrink.Sentence.shrinkSentence)


[<RequireQualifiedAccess>]
module Paragraph =
    let withOptions options =
        let generator =
            MicroGen.Paragraph.generateWithOptions options

        Arb.fromGenShrink (generator, MicroShrink.Paragraph.shrinkRaw)

    let paragraph () =
        Arb.fromGenShrink (MicroGen.Paragraph.generateParagraph, MicroShrink.Paragraph.shrinkParagraph)
