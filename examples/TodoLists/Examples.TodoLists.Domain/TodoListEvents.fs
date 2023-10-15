namespace Examples.TodoLists.Domain

open FSharp.UMX


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
