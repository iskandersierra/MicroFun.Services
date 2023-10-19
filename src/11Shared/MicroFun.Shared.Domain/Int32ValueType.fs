namespace MicroFun.Shared.Domain

open MicroFun
open System.Runtime.CompilerServices
open Validus


[<RequireQualifiedAccess>]
module Int32ValueType =
    [<Literal>]
    let LangErrorMustBePositive =
        "lang:MicroFun.Shared.Domain.Int32ValueType.Error.MustBePositive"

    [<RequireQualifiedAccess>]
    module ErrorMessages =
        let mustBePositive =
            Map.empty
            |> EncodedError.create LangErrorMustBePositive


[<Extension>]
type Int32ValueTypeExtensions() =
    [<Extension>]
    static member IsPositive(this: ValueType.BuilderForValidations<'valueType, int32>) =
        Int32ValueType.ErrorMessages.mustBePositive
        |> EncodedError.format
        |> Fn2.pickFst
        |> Check.WithMessage.Int.greaterThanOrEqualTo 1
        |> this.WithValidator
