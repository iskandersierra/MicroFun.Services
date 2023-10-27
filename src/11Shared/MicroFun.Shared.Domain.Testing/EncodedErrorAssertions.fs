module MicroFun.Shared.Domain.Testing.EncodedErrorAssertions

open MicroFun.Shared.Domain
open Validus
open Xunit // Move to Xunit related library

let assertHasSingleEncodedError (expectedError: EncodedError) (actual: ValidationResult<'a>) =
    match actual with
    | Ok value -> Assert.Fail $"Expected error, but got {value}"
    | Error error ->
        match error |> ValidationErrors.toList with
        | [ actualError ] ->
            let actualError =
                actualError |> EncodedError.tryParse |> Option.get

            Assert.Equal<EncodedError>(expectedError, actualError)
        | errors -> Assert.Fail $"Expected single error, but got {errors.Length} errors: {errors}"
