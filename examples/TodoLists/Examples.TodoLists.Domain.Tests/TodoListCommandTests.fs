module Examples.TodoLists.Domain.Tests.TodoListValidationTests

open System
open MicroFun.Shared.Domain
open MicroFun.Shared.Domain.Testing.ResultAssertions
open Validus
open Xunit

open Examples.TodoLists.Domain


// let assertSameCommandResults (expected: ValidationResult<TodoListCommand>) (actual: ValidationResult<TodoListCommand>) =
//     match actual, expected with
//     | Ok actualCommand, Ok expectedCommand -> Assert.Equal(expectedCommand, actualCommand)
//     | Error e1, Error e2 -> Assert.Equal<ValidationErrors>(e1, e2)
//     | Error error, Ok expectedCommand -> Assert.Fail $"Expected valid command {expectedCommand}, but got error: {error}"
//     | Ok command, Error _ -> Assert.Fail $"Expected error, but got {command}"

[<Theory>]
[<InlineData("")>]
[<InlineData("The title")>]
let ``TodoListCommand.parseCreate`` (title: string) =
    let expected =
        validate {
            let! title = TodoListTitle.valueType.TryParse title
            return TodoListCommand.Create title
        }

    TodoListCommand.parseCreate title
    |> assertResultEqual expected

[<Theory>]
[<InlineData("")>]
[<InlineData("The title")>]
let ``TodoListCommand.parseChangeTitle`` (title: string) =
    let expected =
        validate {
            let! title = TodoListTitle.valueType.TryParse title
            return TodoListCommand.ChangeTitle title
        }

    TodoListCommand.parseChangeTitle title
    |> assertResultEqual expected

[<Fact>]
let ``TodoListCommand.parseArchive`` () =
    let expected =
        validate { return TodoListCommand.Archive }

    TodoListCommand.parseArchive ()
    |> assertResultEqual expected

[<Theory>]
[<InlineData("")>]
[<InlineData("The title")>]
let ``TodoListCommand.parseAddItem`` (title: string) =
    let expected =
        validate {
            let! title = TodoItemTitle.valueType.TryParse title
            return TodoListCommand.AddItem title
        }

    TodoListCommand.parseAddItem title
    |> assertResultEqual expected

[<Theory>]
[<InlineData(-1, "")>]
[<InlineData(-1, "The title")>]
[<InlineData(5, "")>]
[<InlineData(5, "The title")>]
let ``TodoListCommand.parseChangeItemTitle`` (itemId: int) (title: string) =
    let expected =
        validate {
            let! itemId = TodoItemId.valueType.TryParse itemId
            and! title = TodoItemTitle.valueType.TryParse title
            return TodoListCommand.ChangeItemTitle(itemId, title)
        }

    TodoListCommand.parseChangeItemTitle itemId title
    |> assertResultEqual expected

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``TodoListCommand.parseCompleteItem`` (itemId: int) =
    let expected =
        validate {
            let! itemId = TodoItemId.valueType.TryParse itemId
            return TodoListCommand.CompleteItem(itemId)
        }

    TodoListCommand.parseCompleteItem itemId
    |> assertResultEqual expected

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``TodoListCommand.parseReopenItem`` (itemId: int) =
    let expected =
        validate {
            let! itemId = TodoItemId.valueType.TryParse itemId
            return TodoListCommand.ReopenItem(itemId)
        }

    TodoListCommand.parseReopenItem itemId
    |> assertResultEqual expected

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
let ``TodoListCommand.parseArchiveItem`` (itemId: int) =
    let expected =
        validate {
            let! itemId = TodoItemId.valueType.TryParse itemId
            return TodoListCommand.ArchiveItem(itemId)
        }

    TodoListCommand.parseArchiveItem itemId
    |> assertResultEqual expected
