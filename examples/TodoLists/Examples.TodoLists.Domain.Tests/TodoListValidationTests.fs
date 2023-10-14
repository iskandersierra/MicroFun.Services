module TodoListValidationTests

open System
open MicroFun.Shared.Domain
open Validus
open Xunit

open Examples.TodoLists.Domain


[<Theory>]
[<InlineData(null)>]
[<InlineData("")>]
[<InlineData("         ")>]
[<InlineData("Ab")>]
[<InlineData("Too long title                                                                                      .")>]
let ``TodoListTitle.parse invalid`` (title: string) =
    let actual = TodoListTitle.Parse title

    let expectedError =
        EncodedError.create
            StringValueType.LangErrorMustHaveProperLength
            ([ "minLength", TodoListTitleType.MinLength.ToString()
               "maxLength", TodoListTitleType.MaxLength.ToString() ]
             |> Map.ofList)
            TodoListTitleType.FieldName

    match actual with
    | Ok value -> Assert.Fail $"Expected error, but got {value}"
    | Error error ->
        match error |> ValidationErrors.toList with
        | [ actualError ] ->
            let actualError =
                actualError |> EncodedError.tryParse |> Option.get

            Assert.Equal<EncodedError>(expectedError, actualError)
        | errors -> Assert.Fail $"Expected single error, but got {errors.Length} errors: {errors}"

[<Theory>]
[<InlineData("Abc")>]
[<InlineData("Some valid title")>]
[<InlineData("                   Some valid title with trimmable spaces                                             ")>]
[<InlineData("Not too long title                                                                                 .")>]
let ``TodoListTitle.parse valid`` (title: string) =
    let expected = title.Trim()
    let actual = TodoListTitle.Parse title

    match actual with
    | Ok value -> Assert.Equal<string>(expected, TodoListTitle.GetValue value)
    | Error error -> Assert.Fail $"Expected value, but got error: {error}"

[<Fact>]
let ``TodoListTitle.mapValue`` () =
    let title = TodoListTitle.UnsafeParse "valid title"

    let actual =
        title
        |> TodoListTitle.MapValue (sprintf "Some %s")

    let expected =
        TodoListTitle.UnsafeParse "Some valid title"

    Assert.Equal<TodoListTitle>(expected, actual)


[<Theory>]
[<InlineData(null)>]
[<InlineData("")>]
[<InlineData("         ")>]
[<InlineData("Ab")>]
[<InlineData("Too long title                                                                                      .")>]
let ``TodoItemTitle.parse invalid`` (title: string) =
    let actual = TodoItemTitle.parse title

    let expectedError =
        EncodedError.create
            StringValueType.LangErrorMustHaveProperLength
            ([ "minLength", TodoItemTitle.MinLength.ToString()
               "maxLength", TodoItemTitle.MaxLength.ToString() ]
             |> Map.ofList)
            TodoItemTitle.FieldName

    match actual with
    | Ok value -> Assert.Fail $"Expected error, but got {value}"
    | Error error ->
        match error |> ValidationErrors.toList with
        | [ actualError ] ->
            let actualError =
                actualError |> EncodedError.tryParse |> Option.get

            Assert.Equal<EncodedError>(expectedError, actualError)
        | errors -> Assert.Fail $"Expected single error, but got {errors.Length} errors: {errors}"

[<Theory>]
[<InlineData("Abc")>]
[<InlineData("Some valid title")>]
[<InlineData("                   Some valid title with trimmable spaces                                             ")>]
[<InlineData("Not too long title                                                                                 .")>]
let ``TodoItemTitle.parse valid`` (title: string) =
    let expected = title.Trim()
    let actual = TodoItemTitle.parse title

    match actual with
    | Ok value -> Assert.Equal<string>(expected, TodoItemTitle.getValue value)
    | Error error -> Assert.Fail $"Expected value, but got error: {error}"

[<Fact>]
let ``TodoItemTitle.mapValue`` () =
    let title = TodoItemTitle.unsafeParse "valid title"

    let actual =
        title
        |> TodoItemTitle.mapValue (sprintf "Some %s")

    let expected =
        TodoItemTitle.unsafeParse "Some valid title"

    Assert.Equal<TodoItemTitle>(expected, actual)


[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
let ``TodoItemId.parse invalid`` (itemId: int) =
    let actual = TodoItemId.parse itemId

    let expectedError =
        EncodedError.create Int32ValueType.LangErrorMustBePositive Map.empty TodoItemId.FieldName

    match actual with
    | Ok value -> Assert.Fail $"Expected error, but got {value}"
    | Error error ->
        match error |> ValidationErrors.toList with
        | [ actualError ] ->
            let actualError =
                actualError |> EncodedError.tryParse |> Option.get

            Assert.Equal<EncodedError>(expectedError, actualError)
        | errors -> Assert.Fail $"Expected single error, but got {errors.Length} errors: {errors}"

[<Theory>]
[<InlineData(1)>]
[<InlineData(10)>]
[<InlineData(100)>]
let ``TodoItemId.parse valid`` (itemId: int) =
    let expected = itemId
    let actual = TodoItemId.parse itemId

    match actual with
    | Ok value -> Assert.Equal<int>(expected, TodoItemId.getValue value)
    | Error error -> Assert.Fail $"Expected value, but got error: {error}"

[<Fact>]
let ``TodoItemId.mapValue`` () =
    let itemId = TodoItemId.unsafeParse 42
    let actual = itemId |> TodoItemId.mapValue ((+) 10)
    let expected = TodoItemId.unsafeParse 52
    Assert.Equal<TodoItemId>(expected, actual)


let assertSameCommandResults (expected: ValidationResult<TodoListCommand>) (actual: ValidationResult<TodoListCommand>) =
    match actual, expected with
    | Ok actualCommand, Ok expectedCommand -> Assert.Equal(expectedCommand, actualCommand)
    | Error e1, Error e2 -> Assert.Equal<ValidationErrors>(e1, e2)
    | Error error, Ok expectedCommand -> Assert.Fail $"Expected valid command {expectedCommand}, but got error: {error}"
    | Ok command, Error _ -> Assert.Fail $"Expected error, but got {command}"

[<Theory>]
[<InlineData("")>]
[<InlineData("The title")>]
let ``TodoListCommand.parseCreate`` (title: string) =
    let expected =
        validate {
            let! title = TodoListTitle.Parse title
            return TodoListCommand.Create title
        }

    let actual = TodoListCommand.parseCreate title

    assertSameCommandResults expected actual

[<Theory>]
[<InlineData("")>]
[<InlineData("The title")>]
let ``TodoListCommand.parseChangeTitle`` (title: string) =
    let expected =
        validate {
            let! title = TodoListTitle.Parse title
            return TodoListCommand.ChangeTitle title
        }

    let actual = TodoListCommand.parseChangeTitle title

    assertSameCommandResults expected actual

[<Fact>]
let ``TodoListCommand.parseArchive`` () =
    let expected =
        validate { return TodoListCommand.Archive }

    let actual = TodoListCommand.parseArchive ()

    assertSameCommandResults expected actual

[<Theory>]
[<InlineData("")>]
[<InlineData("The title")>]
let ``TodoListCommand.parseAddItem`` (title: string) =
    let expected =
        validate {
            let! title = TodoItemTitle.parse title
            return TodoListCommand.AddItem title
        }

    let actual = TodoListCommand.parseAddItem title

    assertSameCommandResults expected actual

[<Theory>]
[<InlineData(-1, "")>]
[<InlineData(-1, "The title")>]
[<InlineData(5, "")>]
[<InlineData(5, "The title")>]
let ``TodoListCommand.parseChangeItemTitle`` (itemId: int) (title: string) =
    let expected =
        validate {
            let! itemId = TodoItemId.parse itemId
            and! title = TodoItemTitle.parse title
            return TodoListCommand.ChangeItemTitle(itemId, title)
        }

    let actual =
        TodoListCommand.parseChangeItemTitle itemId title

    assertSameCommandResults expected actual

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``TodoListCommand.parseCompleteItem`` (itemId: int) =
    let expected =
        validate {
            let! itemId = TodoItemId.parse itemId
            return TodoListCommand.CompleteItem(itemId)
        }

    let actual = TodoListCommand.parseCompleteItem itemId

    assertSameCommandResults expected actual

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``TodoListCommand.parseReopenItem`` (itemId: int) =
    let expected =
        validate {
            let! itemId = TodoItemId.parse itemId
            return TodoListCommand.ReopenItem(itemId)
        }

    let actual = TodoListCommand.parseReopenItem itemId

    assertSameCommandResults expected actual

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``TodoListCommand.parseArchiveItem`` (itemId: int) =
    let expected =
        validate {
            let! itemId = TodoItemId.parse itemId
            return TodoListCommand.ArchiveItem(itemId)
        }

    let actual = TodoListCommand.parseArchiveItem itemId

    assertSameCommandResults expected actual
