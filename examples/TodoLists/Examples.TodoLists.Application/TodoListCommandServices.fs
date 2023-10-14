namespace Examples.TodoLists.Application

open System.Threading
open System.Threading.Tasks

open MicroFun.Shared.Application


type ITodoListCreateCommandAsyncService =
    abstract ExecuteAsync : command: TodoListCreate * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListChangeTitleCommandAsyncService =
    abstract ExecuteAsync : command: TodoListChangeTitle * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListArchiveCommandAsyncService =
    abstract ExecuteAsync : command: TodoListArchive * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListAddItemCommandAsyncService =
    abstract ExecuteAsync : command: TodoListAddItem * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListChangeItemTitleCommandAsyncService =
    abstract ExecuteAsync : command: TodoListChangeItemTitle * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListCompleteItemCommandAsyncService =
    abstract ExecuteAsync : command: TodoListCompleteItem * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListReopenItemCommandAsyncService =
    abstract ExecuteAsync : command: TodoListReopenItem * cancel: CancellationToken -> Task<CommandAsyncResult>


type ITodoListArchiveItemCommandAsyncService =
    abstract ExecuteAsync : command: TodoListArchiveItem * cancel: CancellationToken -> Task<CommandAsyncResult>
