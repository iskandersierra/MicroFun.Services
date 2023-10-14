namespace Examples.TodoLists.Domain

open FSharp.UMX


type TodoListId = string<todoListId>
and [<Measure>] todoListId

type TodoListTitle = string<todoListTitle>
and [<Measure>] todoListTitle

type TodoItemTitle = string<todoItemTitle>
and [<Measure>] todoItemTitle

type TodoItemId = int<todoItemId>
and [<Measure>] todoItemId


[<RequireQualifiedAccess>]
type TodoListEvent =
    | Created of title: TodoListTitle
    | TitleChanged of title: TodoListTitle
    | Archived

    | ItemAdded of itemId: TodoItemId * title: TodoItemTitle
    | ItemTitleChanged of itemId: TodoItemId * title: TodoItemTitle
    | ItemCompleted of itemId: TodoItemId
    | ItemReopened of itemId: TodoItemId
    | ItemArchived of itemId: TodoItemId


[<RequireQualifiedAccess>]
type TodoListCommand =
    | Create of title: TodoListTitle
    | ChangeTitle of title: TodoListTitle
    | Archive

    | AddItem of title: TodoItemTitle
    | ChangeItemTitle of itemId: TodoItemId * title: TodoItemTitle
    | CompleteItem of itemId: TodoItemId
    | ReopenItem of itemId: TodoItemId
    | ArchiveItem of itemId: TodoItemId
