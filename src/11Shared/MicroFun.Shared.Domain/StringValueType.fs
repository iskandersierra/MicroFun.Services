namespace MicroFun.Shared.Domain

open MicroFun
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Validus


[<RequireQualifiedAccess>]
module StringValueType =
    [<Literal>]
    let DefaultRandomLength = 12

    [<Literal>]
    let LangErrorIsRequired =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.IsRequired"

    [<Literal>]
    let LangErrorMustNotBeShorterThan =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustNotBeShorterThan"

    [<Literal>]
    let LangErrorMustNotBeLongerThan =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustNotBeLongerThan"

    [<Literal>]
    let LangErrorMustHaveLengthBetween =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustHaveLengthBetween"

    [<Literal>]
    let LangErrorMustBeEntityId =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustBeEntityId"

    [<RequireQualifiedAccess>]
    module ErrorMessages =
        let isRequired =
            Map.empty
            |> EncodedError.create LangErrorIsRequired

        let mustNotBeShorterThan (minLength: int) =
            [ "minLength", minLength.ToString() ]
            |> Map.ofList
            |> EncodedError.create LangErrorMustNotBeShorterThan

        let mustNotBeLongerThan (maxLength: int) =
            [ "maxLength", maxLength.ToString() ]
            |> Map.ofList
            |> EncodedError.create LangErrorMustNotBeLongerThan

        let mustHaveLengthBetween (minLength: int) (maxLength: int) =
            [ "minLength", minLength.ToString()
              "maxLength", maxLength.ToString() ]
            |> Map.ofList
            |> EncodedError.create LangErrorMustHaveLengthBetween

        let mustBeEntityId (prefix: string) (randomLength: int) =
            [ "prefix", prefix
              "randomLength", randomLength.ToString() ]
            |> Map.ofList
            |> EncodedError.create LangErrorMustBeEntityId


[<Extension>]
type StringValueTypeExtensions() =
    [<Extension>]
    static member EnsureNotNull(this: ValueType.BuilderForValidations<'valueType, string>) =
        let validator = fun _field -> String.emptyWhenNull >> Ok
        this.WithPrecondition validator

    [<Extension>]
    static member EnsureTrimming(this: ValueType.BuilderForValidations<'valueType, string>) =
        let validator =
            fun _field -> String.emptyWhenNull >> String.trim >> Ok

        this.WithPrecondition validator

    [<Extension>]
    static member IsRequired(this: ValueType.BuilderForValidations<'valueType, string>) =
        this.WithEncodedErrorPrecondition(StringValueType.ErrorMessages.isRequired, Check.WithMessage.String.notEmpty)


    [<Extension>]
    static member MustNotBeShorterThan(this: ValueType.BuilderForValidations<'valueType, string>, minLength: int) =
        this.WithEncodedError(
            StringValueType.ErrorMessages.mustNotBeShorterThan minLength,
            Check.WithMessage.String.greaterThanOrEqualToLen minLength)

    [<Extension>]
    static member MustNotBeLongerThan(this: ValueType.BuilderForValidations<'valueType, string>, maxLength: int) =
        this.WithEncodedError(
            StringValueType.ErrorMessages.mustNotBeLongerThan maxLength,
            Check.WithMessage.String.lessThanOrEqualToLen maxLength)

    [<Extension>]
    static member MustHaveLengthBetween
        (
            this: ValueType.BuilderForValidations<'valueType, string>,
            minLength: int,
            maxLength: int
        ) =
        this.WithEncodedError(
            StringValueType.ErrorMessages.mustHaveLengthBetween minLength maxLength,
            Check.WithMessage.String.betweenLen minLength maxLength)

    [<Extension>]
    static member IsEntityId
        (
            this: ValueType.BuilderForValidations<'valueType, string>,
            prefix: string,
            ?randomLength: int
        ) =
        let randomLength =
            match randomLength with
            | Some length when length <= 0 -> failwithf "randomLength must be greater than 0, given %d" length
            | Some length when length >= 100 -> failwithf "randomLength must be less than 100, given %d" length
            | Some length -> length
            | None -> StringValueType.DefaultRandomLength

        let pattern = $@"^%s{Regex.Escape(prefix)}[a-zA-Z0-9-_]{{%d{randomLength}}}$"

        this.WithEncodedError(
            StringValueType.ErrorMessages.mustBeEntityId prefix randomLength,
            Check.WithMessage.String.pattern pattern)


type IStringValueType<'valueType> =
    inherit IValueType<'valueType, string>

    abstract MinLength: int
    abstract MaxLength: int
    abstract IsOptional: bool


[<RequireQualifiedAccess>]
module TextValueType =
    [<Literal>]
    let MinLength = 1

    [<Literal>]
    let MaxLength = 100

type ITextValueType<'valueType> =
    inherit IStringValueType<'valueType>

type TextValueType<'valueType>
    (
        fieldName: string,
        getValue: 'valueType -> string,
        unsafeParse: string -> 'valueType,
        ?minLength: int,
        ?maxLength: int,
        ?isOptional: bool,
        ?config: ValueType.BuilderForValidations<'valueType, string> -> ValueType.BuilderForValidations<'valueType, string>
    ) =
    inherit CustomValueType<'valueType, string>(fieldName, config = fun builder ->
        let minLength = defaultArg minLength TextValueType.MinLength
        let maxLength = defaultArg maxLength TextValueType.MaxLength
        let isOptional = defaultArg isOptional false
        builder
            .WithConversions(getValue, unsafeParse)
            .EnsureTrimming()
        |> fun b -> if not isOptional then b.IsRequired() else b
        |> fun b -> b.MustHaveLengthBetween(minLength, maxLength)
        |> fun b -> match config with Some config -> config b | _ -> b)

    member this.MinLength = defaultArg minLength TextValueType.MinLength
    member this.MaxLength = defaultArg maxLength TextValueType.MaxLength
    member this.IsOptional = defaultArg isOptional false

    interface ITextValueType<'valueType> with
        member this.MinLength = this.MinLength
        member this.MaxLength = this.MaxLength
        member this.IsOptional = this.IsOptional


[<RequireQualifiedAccess>]
module TitleValueType =
    [<Literal>]
    let MinLength = 3

    [<Literal>]
    let MaxLength = 100

type ITitleValueType<'valueType> =
    inherit ITextValueType<'valueType>

type TitleValueType<'valueType>
    (
        fieldName: string,
        getValue: 'valueType -> string,
        unsafeParse: string -> 'valueType,
        ?minLength: int,
        ?maxLength: int,
        ?isOptional: bool,
        ?config: ValueType.BuilderForValidations<'valueType, string> -> ValueType.BuilderForValidations<'valueType, string>
    ) =
    inherit TextValueType<'valueType>(
        fieldName, getValue, unsafeParse,
        minLength = defaultArg minLength TitleValueType.MinLength,
        maxLength = defaultArg maxLength TitleValueType.MaxLength,
        ?isOptional = isOptional,
        ?config = config)

    interface ITitleValueType<'valueType>


[<RequireQualifiedAccess>]
module DescriptionValueType =
    [<Literal>]
    let MinLength = 1

    [<Literal>]
    let MaxLength = 1000

type IDescriptionValueType<'valueType> =
    inherit ITextValueType<'valueType>

type DescriptionValueType<'valueType>
    (
        fieldName: string,
        getValue: 'valueType -> string,
        unsafeParse: string -> 'valueType,
        ?minLength: int,
        ?maxLength: int,
        ?isOptional: bool,
        ?config: ValueType.BuilderForValidations<'valueType, string> -> ValueType.BuilderForValidations<'valueType, string>
    ) =
    inherit TextValueType<'valueType>(
        fieldName, getValue, unsafeParse,
        minLength = defaultArg minLength DescriptionValueType.MinLength,
        maxLength = defaultArg maxLength DescriptionValueType.MaxLength,
        isOptional = defaultArg isOptional true,
        ?config = config)

    interface IDescriptionValueType<'valueType>


type IEntityIdValueType<'valueType> =
    inherit IValueType<'valueType, string>

    abstract member Prefix: string

type EntityIdValueType<'valueType>
    (
        fieldName: string,
        prefix: string,
        getValue: 'valueType -> string,
        unsafeParse: string -> 'valueType,
        ?config: ValueType.BuilderForValidations<'valueType, string> -> ValueType.BuilderForValidations<'valueType, string>
    ) =
    inherit CustomValueType<'valueType, string>(
        fieldName, getValue, unsafeParse,
        config =
            fun builder ->
                builder.EnsureTrimming().IsEntityId(prefix)
                |> fun b -> match config with Some f -> f b | _ -> b)

    member this.Prefix = prefix

    interface IEntityIdValueType<'valueType> with
        member this.Prefix = this.Prefix
