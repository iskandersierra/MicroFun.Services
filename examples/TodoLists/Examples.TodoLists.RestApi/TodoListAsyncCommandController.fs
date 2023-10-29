namespace Examples.TodoLists.RestApi

open Examples.TodoLists
open Examples.TodoLists.Application
open FsToolkit.ErrorHandling
open Microsoft.AspNetCore.Mvc
open Swashbuckle.AspNetCore.Annotations
open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks


 [<ApiController>]
 [<Route("api/async/todo-list")>]
 type TodoListAsyncCommandController() =
     inherit ControllerBase()

     //[<HttpPost("create", Name = "TodoListAsyncCreate")>]
     //[<SwaggerResponse(200, "Success")>]
     //member this.TodoListAsyncCreate([<FromBody>] body: TodoListCreate, cancel: CancellationToken) =
     //    taskResult {
     //        let! dto = { Application.TodoListCreate.title = ()
     //        ()
     //    }
