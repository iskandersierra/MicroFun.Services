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
    static member MustNotBeShorterThan(this: ValueType.BuilderForValidations<'valueType, string>, minLength: int) =
        StringValueType.ErrorMessages.mustNotBeShorterThan minLength
        |> EncodedError.format
        |> Fn2.pickFst
        |> Check.WithMessage.String.greaterThanOrEqualToLen minLength
        |> this.WithValidator

    [<Extension>]
    static member MustNotBeLongerThan(this: ValueType.BuilderForValidations<'valueType, string>, maxLength: int) =
        StringValueType.ErrorMessages.mustNotBeLongerThan maxLength
        |> EncodedError.format
        |> Fn2.pickFst
        |> Check.WithMessage.String.lessThanOrEqualToLen maxLength
        |> this.WithValidator

    [<Extension>]
    static member MustHaveLengthBetween
        (
            this: ValueType.BuilderForValidations<'valueType, string>,
            minLength: int,
            maxLength: int
        ) =
        StringValueType.ErrorMessages.mustHaveLengthBetween minLength maxLength
        |> EncodedError.format
        |> Fn2.pickFst
        |> Check.WithMessage.String.betweenLen minLength maxLength
        |> this.WithValidator

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

        StringValueType.ErrorMessages.mustBeEntityId prefix randomLength
        |> EncodedError.format
        |> Fn2.pickFst
        |> Check.WithMessage.String.pattern pattern
        |> this.WithValidator
