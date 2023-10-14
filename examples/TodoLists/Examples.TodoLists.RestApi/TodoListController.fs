namespace Examples.TodoLists.RestApi

open Examples.TodoLists
open Examples.TodoLists.Application
open FsToolkit.ErrorHandling
open Microsoft.AspNetCore.Mvc
open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks
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



// [<ApiController>]
// [<Route("api/todo-list")>]
// type TodoListCommandController() =
//     inherit ControllerBase()

//     [<HttpPost("create", Name = "TodoListCreate")>]
//     [<SwaggerResponse>]
//     member this.TodoListCreate([<FromBody>] body: TodoListCreate, cancel: CancellationToken) =
//         taskResult {
//             let! dto = { Application.TodoListCreate.title = ()
//             ()
//         }
