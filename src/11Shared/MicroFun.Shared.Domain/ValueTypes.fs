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
    let LangErrorMustNotBeShorterThan =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustNotBeShorterThan"

    [<Literal>]
    let LangErrorMustNotBeLongerThan =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustNotBeLongerThan"

    [<Literal>]
    let LangErrorMustHaveLengthBetween =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustHaveLengthBetween"

    [<RequireQualifiedAccess>]
    module WithMessage =
        let mustHaveProperLengthValidator minLength maxLength message : Validator<string, string> =
            Check.WithMessage.String.betweenLen minLength maxLength message

    let mustHaveProperLengthValidator minLength maxLength =
        let data =
            [ "minLength", minLength.ToString()
              "maxLength", maxLength.ToString() ]
            |> Map.ofList

        WithMessage.mustHaveProperLengthValidator
            minLength
            maxLength
            (EncodedError.prepareFormatted LangErrorMustHaveLengthBetween data)

    let parseTrimmed (field: string) (unsafeParse: string -> 'valueType) (validator: Validator<string, string>) =
        String.emptyWhenNull
        >> String.trim
        >> ValueType.parse field unsafeParse validator


[<Extension>]
type StringValueTypeExtensions() =
    [<Extension>]
    static member EnsureString(this: ValueType.BuilderForValidations<'valueType, string>) =
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

[<RequireQualifiedAccess>]
module EntityIdValueType =
    [<Literal>]
    let SuffixPattern = "[a-zA-Z0-9-]+"

    [<Literal>]
    let SuffixMinLength = 1

    [<Literal>]
    let SuffixMaxLength = 32

    [<Literal>]
    let LangErrorMustHaveProperPrefix =
        "lang:MicroFun.Shared.Domain.EntityIdValueType.Error.MustHaveProperPrefix"


    type ValidatorOptions =
        { minLength: int
          maxLength: int
          pattern: string }

    type PrefixedValidatorOptions =
        { suffixMinLength: int
          suffixMaxLength: int
          suffixPattern: string }


    [<RequireQualifiedAccess>]
    module WithMessage =
        let validatorWith (options: ValidatorOptions) message : Validator<string, string> =
            let lengthValidator =
                StringValueType.mustHaveProperLengthValidator options.minLength options.maxLength

            let prefixValidator =
                Check.WithMessage.String.pattern options.pattern message

            ValidatorGroup(lengthValidator)
                .And(prefixValidator)
                .Build()

        let validator =
            validatorWith
                { minLength = SuffixMinLength
                  maxLength = SuffixMaxLength
                  pattern = $"^{SuffixPattern}$" }

        let prefixedValidatorWith
            (options: PrefixedValidatorOptions)
            (prefix: string)
            message
            : Validator<string, string> =
            if String.isNullOrWhiteSpace prefix then
                failwith "prefix cannot be null or whitespace"

            validatorWith
                { minLength = prefix.Length + options.suffixMinLength
                  maxLength = prefix.Length + options.suffixMaxLength
                  pattern = $"^{Regex.Escape(prefix)}{options.suffixPattern}$" }
                message

        let prefixedValidator =
            prefixedValidatorWith
                { suffixMinLength = SuffixMinLength
                  suffixMaxLength = SuffixMaxLength
                  suffixPattern = SuffixPattern }

    let validatorWith options prefix =
        let data =
            [ "prefix", prefix
              "minLength", options.minLength.ToString()
              "maxLength", options.maxLength.ToString()
              "pattern", options.pattern ]
            |> Map.ofList

        WithMessage.validatorWith options (EncodedError.prepareFormatted LangErrorMustHaveProperPrefix data)

    let validator prefix =
        let data = [ "prefix", prefix ] |> Map.ofList
        WithMessage.validator (EncodedError.prepareFormatted LangErrorMustHaveProperPrefix data)

    let prefixedValidatorWith options prefix =
        let data =
            [ "prefix", prefix
              "suffixMinLength", options.suffixMinLength.ToString()
              "suffixMaxLength", options.suffixMaxLength.ToString()
              "suffixPattern", options.suffixPattern ]
            |> Map.ofList

        WithMessage.prefixedValidatorWith
            options
            prefix
            (EncodedError.prepareFormatted LangErrorMustHaveProperPrefix data)

    let prefixedValidator prefix =
        let data = [ "prefix", prefix ] |> Map.ofList
        WithMessage.prefixedValidator prefix (EncodedError.prepareFormatted LangErrorMustHaveProperPrefix data)

    let parseTrimmed (field: string) = StringValueType.parseTrimmed field


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
