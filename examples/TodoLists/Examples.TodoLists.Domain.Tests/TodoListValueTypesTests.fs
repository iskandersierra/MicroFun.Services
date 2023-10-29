module Examples.TodoLists.Domain.Tests.TodoListValueTypesTests

open MicroFun.Shared.Domain
open Xunit
open MicroFun.Shared.Domain.Testing.ResultAssertions
open MicroFun.Shared.Domain.Testing.EncodedErrorAssertions

open Examples.TodoLists.Domain


[<Theory>]
[<InlineData(null)>]
[<InlineData("")>]
[<InlineData("         ")>]
let ``TodoListTitle.parse invalid is required`` (title: string) =
    let actual = TodoListTitle.valueType.TryParse title

    let expectedError = StringValueType.ErrorMessages.isRequired

    actual
    |> assertHasSingleEncodedError expectedError

[<Theory>]
[<InlineData("Ab")>]
[<InlineData("Too long title                                                                                      .")>]
let ``TodoListTitle.parse invalid`` (title: string) =
    let actual = TodoListTitle.valueType.TryParse title

    let expectedError =
        StringValueType.ErrorMessages.mustHaveLengthBetween
            TitleValueType.MinLength
            TitleValueType.MaxLength

    actual
    |> assertHasSingleEncodedError expectedError

[<Theory>]
[<InlineData("Abc")>]
[<InlineData("Some valid title")>]
[<InlineData("                   Some valid title with trimmable spaces                                             ")>]
[<InlineData("Not too long title                                                                                 .")>]
let ``TodoListTitle.parse valid`` (title: string) =
    let expected = title.Trim() |> TodoListTitle.valueType.UnsafeParse
    let actual = TodoListTitle.valueType.TryParse title

    actual
    |> assertResultOk expected


[<Theory>]
[<InlineData(null)>]
[<InlineData("")>]
[<InlineData("         ")>]
let ``TodoItemTitle.parse invalid is required`` (title: string) =
    let actual = TodoItemTitle.valueType.TryParse title

    let expectedError = StringValueType.ErrorMessages.isRequired

    actual
    |> assertHasSingleEncodedError expectedError

[<Theory>]
[<InlineData("Ab")>]
[<InlineData("Too long title                                                                                      .")>]
let ``TodoItemTitle.parse invalid length`` (title: string) =
    let actual = TodoItemTitle.valueType.TryParse title

    let expectedError =
        StringValueType.ErrorMessages.mustHaveLengthBetween
            TitleValueType.MinLength
            TitleValueType.MaxLength

    actual
    |> assertHasSingleEncodedError expectedError

[<Theory>]
[<InlineData("Abc")>]
[<InlineData("Some valid title")>]
[<InlineData("                   Some valid title with trimmable spaces                                             ")>]
[<InlineData("Not too long title                                                                                 .")>]
let ``TodoItemTitle.parse valid`` (title: string) =
    let expected = title.Trim() |> TodoItemTitle.valueType.UnsafeParse
    let actual = TodoItemTitle.valueType.TryParse title

    actual
    |> assertResultOk expected


[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
let ``TodoItemId.parse invalid`` (itemId: int) =
    let actual = TodoItemId.valueType.TryParse itemId
    let expectedError = Int32ValueType.ErrorMessages.mustBePositive

    actual
    |> assertHasSingleEncodedError expectedError

[<Theory>]
[<InlineData(1)>]
[<InlineData(10)>]
[<InlineData(100)>]
let ``TodoItemId.parse valid`` (itemId: int) =
    let expected = itemId |> TodoItemId.valueType.UnsafeParse
    let actual = TodoItemId.valueType.TryParse itemId

    actual
    |> assertResultOk expected
