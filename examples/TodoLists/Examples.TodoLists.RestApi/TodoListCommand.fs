namespace Examples.TodoLists.RestApi

open Examples.TodoLists
open Examples.TodoLists.Application
open Validus


[<CLIMutable>]
type TodoListCreate = { title: string }

[<CLIMutable>]
type TodoListChangeTitle = { title: string }

[<CLIMutable>]
type TodoListAddItem = { title: string }

[<CLIMutable>]
type TodoListChangeItemTitle = { itemId: int; title: string }

[<CLIMutable>]
type TodoListCompleteItem = { itemId: int }

[<CLIMutable>]
type TodoListReopenItem = { itemId: int }

[<CLIMutable>]
type TodoListArchiveItem = { itemId: int }


[<RequireQualifiedAccess>]
module TodoListCommand =
    let toApplicationCreate (body: TodoListCreate) : ValidationResult<Application.TodoListCreate> =
        validate {
            return { title = body.title }
        }
