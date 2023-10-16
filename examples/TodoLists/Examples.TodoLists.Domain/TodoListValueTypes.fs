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


module TodoListIdType =
    [<Literal>]
    let FieldName = "TodoListId"
    [<Literal>]
    let Prefix = "tdls-"

module TodoListTitleType =
    [<Literal>]
    let FieldName = "TodoListTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

module TodoItemTitleType =
    [<Literal>]
    let FieldName = "TodoItemTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

module TodoItemIdType =
    [<Literal>]
    let FieldName = "TodoItemId"


[<AutoOpen>]
module TodoListValueTypes =
    let TodoListId =
        let getValue (value: TodoListId) = UMX.untag value
        ValueType
            .Builder<TodoListId, string>(TodoListIdType.FieldName)
            .WithConversions(getValue, UMX.tag<todoListId>)
            .EnsureTrimming()
            .IsEntityId(TodoListIdType.Prefix)
            .Create()

    let TodoListTitle =
        let getValue (value: TodoListTitle) = UMX.untag value
        ValueType
            .Builder<TodoListTitle, string>(TodoListTitleType.FieldName)
            .WithConversions(getValue, UMX.tag<todoListTitle>)
            .EnsureTrimming()
            .MustHaveLengthBetween(TodoListTitleType.MinLength, TodoListTitleType.MaxLength)
            .Create()

    let TodoItemTitle =
        let getValue (value: TodoItemTitle) = UMX.untag value
        ValueType
            .Builder<TodoItemTitle, string>(TodoItemTitleType.FieldName)
            .WithConversions(getValue, UMX.tag<todoItemTitle>)
            .EnsureTrimming()
            .MustHaveLengthBetween(TodoItemTitleType.MinLength, TodoItemTitleType.MaxLength)
            .Create()

    let TodoItemId =
        let getValue (value: TodoItemId) = UMX.untag value
        ValueType
            .Builder<TodoItemId, int>(TodoItemIdType.FieldName)
            .WithConversions(getValue, UMX.tag<todoItemId>)
            .IsPositive()
            .Create()
