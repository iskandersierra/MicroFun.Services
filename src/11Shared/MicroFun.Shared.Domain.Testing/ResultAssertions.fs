module MicroFun.Shared.Domain.Testing.ResultAssertions

open Xunit

let assertResultOk (expected: 'a) (actual: Result<'a, 'e>) =
    match actual with
    | Ok value -> Assert.Equal<'a>(expected, value)
    | Error error -> Assert.Fail $"Expected value {expected}, but got error: {error}"

let assertResultEqual (expected: Result<'a, 'e>) (actual: Result<'a, 'e>) =
    match actual, expected with
    | Ok actual, Ok expected -> Assert.Equal<'a>(expected, actual)
    | Error actualError, Error expectedError -> Assert.Equal<'e>(actualError, expectedError)
    | Error actualError, Ok expected -> Assert.Fail $"Expected value {expected}, but got error: {actualError}"
    | Ok actual, Error expectedError -> Assert.Fail $"Expected error {expectedError}, but got value {actual}"
