namespace Examples.TodoLists.Domain

open FSharp.UMX
open MicroFun.Shared.Domain


type TodoListId = string<todoListId>
and [<Measure>] todoListId

type TodoListTitle = string<todoListTitle>
and [<Measure>] todoListTitle

type TodoItemTitle = string<todoItemTitle>
and [<Measure>] todoItemTitle

type TodoItemId = int<todoItemId>
and [<Measure>] todoItemId


[<RequireQualifiedAccess>]
module TodoListId =
    [<Literal>]
    let FieldName = "TodoListId"

    [<Literal>]
    let Prefix = "tdls-"

    let inline private getValue (value: TodoListId) = UMX.untag value

    type ValueType() =
        inherit EntityIdValueType<TodoListId>(FieldName, Prefix, getValue, UMX.tag<todoListId>)


    let valueType = ValueType()


[<RequireQualifiedAccess>]
module TodoListTitle =
    [<Literal>]
    let FieldName = "TodoListTitle"

    type ValueType() =
        inherit TitleValueType<TodoListTitle>(FieldName, UMX.untag, UMX.tag<todoListTitle>)

    let valueType = ValueType()


[<RequireQualifiedAccess>]
module TodoItemTitle =
    [<Literal>]
    let FieldName = "TodoItemTitle"

    type ValueType() =
        inherit TitleValueType<TodoItemTitle>(FieldName, UMX.untag, UMX.tag<todoItemTitle>)

    let valueType = ValueType()


[<RequireQualifiedAccess>]
module TodoItemId =
    [<Literal>]
    let FieldName = "TodoItemId"

    type ValueType() =
        inherit CustomValueType<TodoItemId, int>
            (FieldName, UMX.untag, UMX.tag<todoItemId>, fun b -> b.IsPositive())

    let valueType = ValueType()
