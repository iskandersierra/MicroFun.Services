namespace MicroFun.Shared.Domain

open MicroFun
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Validus

exception ValidationErrorsException of ValidationErrors


type IValueType<'valueType, 'underlying> =
    abstract FieldName : string

    abstract GetValue : 'valueType -> 'underlying
    abstract UnsafeParse : 'underlying -> 'valueType

    abstract Validator : Validator<'underlying, 'underlying>

    abstract TryParseAs : field: string -> value: 'underlying -> ValidationResult<'valueType>
    abstract TryParse : value: 'underlying -> ValidationResult<'valueType>

    abstract ParseAs : field: string -> value: 'underlying -> 'valueType
    abstract Parse : value: 'underlying -> 'valueType

    abstract TryBindValueAs : field: string -> f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> ValidationResult<'valueType>
    abstract BindValueAs : field: string -> f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> 'valueType

    abstract TryBindValue : f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> ValidationResult<'valueType>
    abstract BindValue : f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> 'valueType

    abstract TryMapValueAs : field: string -> f: ('underlying -> 'underlying) -> 'valueType -> ValidationResult<'valueType>
    abstract MapValueAs : field: string -> f: ('underlying -> 'underlying) -> 'valueType -> 'valueType

    abstract TryMapValue : f: ('underlying -> 'underlying) -> 'valueType -> ValidationResult<'valueType>
    abstract MapValue : f: ('underlying -> 'underlying) -> 'valueType -> 'valueType


[<RequireQualifiedAccess>]
module ValueType =
    let parse
        (field: string)
        (unsafeParse: 'underlying -> 'valueType)
        (validator: Validator<'underlying, 'underlying>)
        (value: 'underlying)
        : ValidationResult<'valueType> =
        validate {
            let! value = validator field value
            return unsafeParse value
        }

    let valueMapper
        (getValue: 'valueType -> 'underlying)
        (unsafeParse: 'underlying -> 'valueType)
        (f: 'underlying -> 'underlying)
        =
        getValue >> f >> unsafeParse

    let create
        (fieldName: string)
        (getValue: 'valueType -> 'underlying)
        (unsafeParse: 'underlying -> 'valueType)
        (validator: Validator<'underlying, 'underlying>)
        =
        let failWithErrors result = result |> Result.getOr (ValidationErrorsException >> raise)

        let tryParse field value =
            validate {
                let! value = validator field value
                return unsafeParse value
            }

        let parse field = tryParse field >> failWithErrors

        let tryBindValue field f = getValue >> f >> Result.bind (tryParse field)

        let bindValue field f = tryBindValue field f >> failWithErrors

        let tryMapValue field f = tryBindValue field (f >> Ok)

        let mapValue field f = tryMapValue field f >> failWithErrors

        { new IValueType<'valueType, 'underlying> with
            member _.FieldName = fieldName
            member _.GetValue value = getValue value
            member _.UnsafeParse underlying = unsafeParse underlying
            member _.Validator = validator
            member _.TryParseAs field value = tryParse field value
            member _.TryParse value = tryParse fieldName value
            member _.ParseAs field value = parse field value
            member _.Parse value = parse fieldName value
            member _.TryBindValueAs field f value = tryBindValue field f value
            member _.BindValueAs field f value = bindValue field f value
            member _.TryBindValue f value = tryBindValue fieldName f value
            member _.BindValue f value = bindValue fieldName f value
            member _.TryMapValueAs field f value = tryMapValue field f value
            member _.MapValueAs field f value = mapValue field f value
            member _.TryMapValue f value = tryMapValue fieldName f value
            member _.MapValue f value = mapValue fieldName f value }

    let combineValidatorsWith
        (fn: ValidatorGroup<'a> -> Validator<'a, 'a> -> ValidatorGroup<'a>)
        (validators: Validator<'a, 'a> seq)
        =
        validators
        |> Seq.fold
            (fun acc validator ->
                match acc with
                | None -> Some(ValidatorGroup(validator))
                | Some group -> Some(fn group validator))
            None
        |> Option.map (fun group -> group.Build())
        |> Option.defaultValue Validator.success

    let combineValidators (validators: Validator<'a, 'a> seq) =
        combineValidatorsWith (fun group validator -> group.And validator) validators

    let combineValidatorsSequence (validators: Validator<'a, 'a> seq) =
        combineValidatorsWith (fun group validator -> group.Then validator) validators

    let createValidatorFrom (preconditions: Validator<'a, 'a> seq) (validators: Validator<'a, 'a> seq) =
        seq {
            yield! preconditions
            yield combineValidators validators
        }
        |> combineValidatorsSequence


    type BuilderForConversion<'valueType, 'underlying>(fieldName: string) =
        member this.WithConversions(getValue, unsafeParse) =
            BuilderForValidations(fieldName, getValue, unsafeParse)

    and BuilderForValidations<'valueType, 'underlying>
        (
            fieldName: string,
            getValue: 'valueType -> 'underlying,
            unsafeParse: 'underlying -> 'valueType
        ) =
        let preconditions = ResizeArray()
        let validators = ResizeArray()

        member this.WithPrecondition(validator: Validator<'underlying, 'underlying>) =
            preconditions.Add validator
            this

        member this.WithValidator(validator: Validator<'underlying, 'underlying>) =
            validators.Add validator
            this

        member this.Create() =
            createValidatorFrom preconditions validators
            |> create fieldName getValue unsafeParse

    let Builder<'valueType, 'underlying> fieldName =
        BuilderForConversion<'valueType, 'underlying> fieldName


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
        let data =
            [ "minLength", minLength.ToString() ]
            |> Map.ofList

        let message =
            EncodedError.prepareFormatted StringValueType.LangErrorMustNotBeShorterThan data

        let validator =
            Check.WithMessage.String.greaterThanOrEqualToLen minLength message

        this.WithValidator validator

    [<Extension>]
    static member MustNotBeLongerThan(this: ValueType.BuilderForValidations<'valueType, string>, maxLength: int) =
        let data =
            [ "maxLength", maxLength.ToString() ]
            |> Map.ofList

        let message =
            EncodedError.prepareFormatted StringValueType.LangErrorMustNotBeLongerThan data

        let validator =
            Check.WithMessage.String.greaterThanOrEqualToLen maxLength message

        this.WithValidator validator

    [<Extension>]
    static member MustHaveLengthBetween
        (
            this: ValueType.BuilderForValidations<'valueType, string>,
            minLength: int,
            maxLength: int
        ) =
        let data =
            [ "minLength", minLength.ToString()
              "maxLength", maxLength.ToString() ]
            |> Map.ofList

        let message =
            EncodedError.prepareFormatted StringValueType.LangErrorMustHaveLengthBetween data

        let validator =
            Check.WithMessage.String.betweenLen minLength maxLength message

        this.WithValidator validator

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

        let data =
            [ "prefix", prefix
              "randomLength", randomLength.ToString() ]
            |> Map.ofList

        let message =
            EncodedError.prepareFormatted StringValueType.LangErrorMustBeEntityId data

        let validator =
            Check.WithMessage.String.pattern pattern message

        this.WithValidator validator


[<RequireQualifiedAccess>]
module Int32ValueType =
    [<Literal>]
    let LangErrorMustBePositive =
        "lang:MicroFun.Shared.Domain.Int32ValueType.Error.MustBePositive"

[<Extension>]
type Int32ValueTypeExtensions() =
    [<Extension>]
    static member IsPositive(this: ValueType.BuilderForValidations<'valueType, int32>) =
        let data = Map.empty

        let message =
            EncodedError.prepareFormatted Int32ValueType.LangErrorMustBePositive data

        let validator =
            Check.WithMessage.Int.greaterThanOrEqualTo 1 message

        this.WithValidator validator
