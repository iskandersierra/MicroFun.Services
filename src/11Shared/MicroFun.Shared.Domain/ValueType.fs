namespace MicroFun.Shared.Domain

open MicroFun
open System.Runtime.CompilerServices
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

    abstract TryBindValueAs :
        field: string -> f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> ValidationResult<'valueType>

    abstract BindValueAs :
        field: string -> f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> 'valueType

    abstract TryBindValue :
        f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> ValidationResult<'valueType>

    abstract BindValue : f: ('underlying -> ValidationResult<'underlying>) -> 'valueType -> 'valueType

    abstract TryMapValueAs :
        field: string -> f: ('underlying -> 'underlying) -> 'valueType -> ValidationResult<'valueType>

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
        let failWithErrors result =
            result
            |> Result.getOr (ValidationErrorsException >> raise)

        let tryParse field value =
            validate {
                let! value = validator field value
                return unsafeParse value
            }

        let parse field = tryParse field >> failWithErrors

        let tryBindValue field f =
            getValue >> f >> Result.bind (tryParse field)

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


type CustomValueType<'valueType, 'underlying>(factory: unit -> IValueType<'valueType, 'underlying>) =

    let inner = factory ()

    new(fieldName: string,
        config: ValueType.BuilderForConversion<'valueType, 'underlying>
                    -> ValueType.BuilderForValidations<'valueType, 'underlying>) =
        let factory () =
            let builder = ValueType.Builder<'valueType, 'underlying> fieldName
            let builder = config builder
            builder.Create()

        CustomValueType(factory)

    new(fieldName: string,
        getValue: 'valueType -> 'underlying,
        unsafeParse: 'underlying -> 'valueType,
        config: ValueType.BuilderForValidations<'valueType, 'underlying>
                    -> ValueType.BuilderForValidations<'valueType, 'underlying>) =

        CustomValueType(fieldName, fun builder ->
            builder.WithConversions(getValue, unsafeParse) |> config)


    member this.FieldName = inner.FieldName
    member this.GetValue value = inner.GetValue value
    member this.UnsafeParse underlying = inner.UnsafeParse underlying
    member this.Validator = inner.Validator
    member this.TryParseAs field value = inner.TryParseAs field value
    member this.TryParse value = inner.TryParse value
    member this.ParseAs field value = inner.ParseAs field value
    member this.Parse value = inner.Parse value
    member this.TryBindValueAs field f value = inner.TryBindValueAs field f value
    member this.BindValueAs field f value = inner.BindValueAs field f value
    member this.TryBindValue f value = inner.TryBindValue f value
    member this.BindValue f value = inner.BindValue f value
    member this.TryMapValueAs field f value = inner.TryMapValueAs field f value
    member this.MapValueAs field f value = inner.MapValueAs field f value
    member this.TryMapValue f value = inner.TryMapValue f value
    member this.MapValue f value = inner.MapValue f value

    interface IValueType<'valueType, 'underlying> with
        member this.FieldName = inner.FieldName
        member this.GetValue value = inner.GetValue value
        member this.UnsafeParse underlying = inner.UnsafeParse underlying
        member this.Validator = inner.Validator
        member this.TryParseAs field value = inner.TryParseAs field value
        member this.TryParse value = inner.TryParse value
        member this.ParseAs field value = inner.ParseAs field value
        member this.Parse value = inner.Parse value
        member this.TryBindValueAs field f value = inner.TryBindValueAs field f value
        member this.BindValueAs field f value = inner.BindValueAs field f value
        member this.TryBindValue f value = inner.TryBindValue f value
        member this.BindValue f value = inner.BindValue f value
        member this.TryMapValueAs field f value = inner.TryMapValueAs field f value
        member this.MapValueAs field f value = inner.MapValueAs field f value
        member this.TryMapValue f value = inner.TryMapValue f value
        member this.MapValue f value = inner.MapValue f value


[<Extension>]
type ValueTypeExtensions() =
    [<Extension>]
    static member WithEncodedErrorPrecondition
        (
            this: ValueType.BuilderForValidations<'valueType, 'underlying>,
            encodedError: EncodedError,
            validator: ValidationMessage -> Validator<'underlying, 'underlying>
        ) =
        encodedError
        |> EncodedError.format
        |> Fn2.pickFst
        |> validator
        |> this.WithPrecondition

    [<Extension>]
    static member WithEncodedError
        (
            this: ValueType.BuilderForValidations<'valueType, 'underlying>,
            encodedError: EncodedError,
            validator: ValidationMessage -> Validator<'underlying, 'underlying>
        ) =
        encodedError
        |> EncodedError.format
        |> Fn2.pickFst
        |> validator
        |> this.WithValidator
