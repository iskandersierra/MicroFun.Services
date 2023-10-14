namespace MicroFun.Shared.Domain

open FSharp.UMX
open MicroFun
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Validus

[<Struct>]
type EncodedError =
    { code: string
      data: Map<string, string>
      field: string }

[<RequireQualifiedAccess>]
module EncodedError =
    [<Literal>]
    let DataSeparator = "|"

    let KVSeparator = "="

    let create code data field =
        { code = code
          data = data
          field = field }

    let format (error: EncodedError) =
        stringBuilder {
            yield error.code
            yield DataSeparator
            yield "field="
            yield error.field

            for (key, value) in error.data |> Map.toSeq do
                yield DataSeparator
                yield key
                yield "="
                yield value
        }

    let formattedRegex =
        let sep = Regex.Escape DataSeparator
        let kvsep = Regex.Escape KVSeparator
        let code = $@"(?<code>[^{sep}]+)"
        let field = $@"{sep}field{kvsep}(?<field>[^{sep}]+)"

        let kvp =
            $@"{sep}(?<key>[^{kvsep}{sep}]+){kvsep}(?<value>[^{sep}]+)"

        Regex($@"^{code}{field}({kvp})*$", RegexOptions.Compiled)

    let tryParse (value: string) =
        let match' = formattedRegex.Match value

        if match'.Success then
            let code = match'.Groups.["code"].Value
            let field = match'.Groups.["field"].Value

            let data =
                let keys =
                    match'.Groups.["key"].Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun m -> m.Value)

                let values =
                    match'.Groups.["value"].Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun m -> m.Value)

                Seq.zip keys values |> Map.ofSeq

            Some
                { code = code
                  data = data
                  field = field }
        else
            None

    let prepareFormatted code data = create code data >> format


type IValueType<'valueType, 'underlying> =
    abstract FieldName : string

    abstract GetValue : 'valueType -> 'underlying
    abstract UnsafeParse : 'underlying -> 'valueType

    abstract Validator : Validator<'underlying, 'underlying>

    abstract ParseAs : field: string -> value: 'underlying -> ValidationResult<'valueType>
    abstract Parse : value: 'underlying -> ValidationResult<'valueType>

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
        let parse field value =
            validate {
                let! value = validator field value
                return unsafeParse value
            }

        let mapValue f = getValue >> f >> unsafeParse

        { new IValueType<'valueType, 'underlying> with
            member this.FieldName = fieldName
            member this.GetValue value = getValue value
            member this.UnsafeParse underlying = unsafeParse underlying
            member this.Validator = validator
            member this.ParseAs field value = parse field value
            member this.Parse value = parse fieldName value
            member this.MapValue f value = mapValue f value }

    type BuilderForConversion<'valueType, 'underlying> internal (fieldName: string) =
        member this.WithConversions(getValue, unsafeParse) =
            BuilderForValidator(fieldName, getValue, unsafeParse)

    and BuilderForValidator<'valueType, 'underlying>
        internal
        (
            fieldName: string,
            getValue: 'valueType -> 'underlying,
            unsafeParse: 'underlying -> 'valueType
        ) =
        member this.WithValidator(validator) =
            BuilderForCreate(fieldName, getValue, unsafeParse, validator)

    and BuilderForCreate<'valueType, 'underlying>
        internal
        (
            fieldName: string,
            getValue: 'valueType -> 'underlying,
            unsafeParse: 'underlying -> 'valueType,
            validator: Validator<'underlying, 'underlying>
        ) =
        member this.Create() = create fieldName getValue unsafeParse validator

    let Builder fieldName = BuilderForConversion fieldName


[<RequireQualifiedAccess>]
module StringValueType =
    [<Literal>]
    let LangErrorMustHaveProperLength =
        "lang:MicroFun.Shared.Domain.StringValueType.Error.MustHaveProperLength"

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
            (EncodedError.prepareFormatted LangErrorMustHaveProperLength data)

    let parseTrimmed (field: string) (unsafeParse: string -> 'valueType) (validator: Validator<string, string>) =
        String.emptyWhenNull
        >> String.trim
        >> ValueType.parse field unsafeParse validator


    type BuilderForConversion<'valueType> internal (fieldName: string) =
        member this.WithConversions(getValue, unsafeParse) =
            BuilderForValidator(fieldName, getValue, unsafeParse)

    and BuilderForValidator<'valueType>
        internal
        (
            fieldName: string,
            getValue: 'valueType -> string,
            unsafeParse: string -> 'valueType
        ) =
        member this.WithValidator(validator) =
            BuilderForCreate(fieldName, getValue, unsafeParse, validator)

        member this.WithMinLength(value) =
            BuilderForOptionsValidator(fieldName, getValue, unsafeParse, Some value, None, false)

        member this.WithMaxLength(value) =
            BuilderForOptionsValidator(fieldName, getValue, unsafeParse, None, Some value, false)

        member this.WithLengthBetween(min, max) =
            BuilderForOptionsValidator(fieldName, getValue, unsafeParse, Some min, Some max, false)

        member this.WithTrimming() =
            BuilderForOptionsValidator(fieldName, getValue, unsafeParse, None, None, true)

    and BuilderForOptionsValidator<'valueType>
        internal
        (
            fieldName: string,
            getValue: 'valueType -> string,
            unsafeParse: string -> 'valueType,
            minLength: int option,
            maxLength: int option,
            trimming: bool
        ) =
        let mutable minLength = minLength
        let mutable maxLength = maxLength
        let mutable trimming = trimming

        member this.WithMinLength(value) =
            minLength <- Some value
            this

        member this.WithMaxLength(value) =
            maxLength <- Some value
            this

        member this.WithLengthBetween(min, max) =
            minLength <- Some min
            maxLength <- Some max
            this

        member this.WithTrimming() =
            trimming <- true
            this

        member this.Create() =
            let validator =
                let cleaner =
                    if trimming then
                        String.emptyWhenNull >> String.trim
                    else
                        String.emptyWhenNull

                let validator =
                    match minLength, maxLength with
                    | Some minLength, Some maxLength ->
                        let data =
                            [ "minLength", minLength.ToString()
                              "maxLength", maxLength.ToString() ]
                            |> Map.ofList
                        let message = EncodedError.prepareFormatted LangErrorMustHaveProperLength data
                        Check.WithMessage.String.betweenLen minLength maxLength message

                    | Some minLength, None ->
                        let data =
                            [ "minLength", minLength.ToString() ]
                            |> Map.ofList
                        let message = EncodedError.prepareFormatted LangErrorMustHaveProperLength data
                        Check.WithMessage.String.greaterThanOrEqualToLen minLength message

                    | None, Some maxLength ->
                        let data =
                            [ "maxLength", maxLength.ToString() ]
                            |> Map.ofList
                        let message = EncodedError.prepareFormatted LangErrorMustHaveProperLength data
                        Check.WithMessage.String.lessThanOrEqualToLen maxLength message

                    | None, None ->
                        Validator.success

                fun field -> cleaner >> validator field

            ValueType.create fieldName getValue unsafeParse validator

    and BuilderForCreate<'valueType>
        internal
        (
            fieldName: string,
            getValue: 'valueType -> string,
            unsafeParse: string -> 'valueType,
            validator: Validator<string, string>
        ) =
        member this.Create() =
            ValueType.create fieldName getValue unsafeParse validator

    let Builder<'valueType> fieldName = BuilderForConversion<'valueType> fieldName

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

    [<RequireQualifiedAccess>]
    module WithMessage =
        let mustBePositiveValidator message : Validator<int, int> =
            Check.WithMessage.Int.greaterThanOrEqualTo 1 message

    let mustBePositiveValidator =
        let data = Map.empty
        WithMessage.mustBePositiveValidator (EncodedError.prepareFormatted LangErrorMustBePositive data)
