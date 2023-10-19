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

    let valueType =
        let getValue (value: TodoListId) = UMX.untag value
        ValueType
            .Builder<TodoListId, string>(FieldName)
            .WithConversions(getValue, UMX.tag<todoListId>)
            .EnsureTrimming()
            .IsEntityId(Prefix)
            .Create()


[<RequireQualifiedAccess>]
module TodoListTitle =
    [<Literal>]
    let FieldName = "TodoListTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

    let valueType =
        let getValue (value: TodoListTitle) = UMX.untag value
        ValueType
            .Builder<TodoListTitle, string>(FieldName)
            .WithConversions(getValue, UMX.tag<todoListTitle>)
            .EnsureTrimming()
            .MustHaveLengthBetween(MinLength, MaxLength)
            .Create()


[<RequireQualifiedAccess>]
module TodoItemTitle =
    [<Literal>]
    let FieldName = "TodoItemTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

    let valueType =
        let getValue (value: TodoItemTitle) = UMX.untag value
        ValueType
            .Builder<TodoItemTitle, string>(FieldName)
            .WithConversions(getValue, UMX.tag<todoItemTitle>)
            .EnsureTrimming()
            .MustHaveLengthBetween(MinLength, MaxLength)
            .Create()


[<RequireQualifiedAccess>]
module TodoItemId =
    [<Literal>]
    let FieldName = "TodoItemId"

    let valueType =
        let getValue (value: TodoItemId) = UMX.untag value
        ValueType
            .Builder<TodoItemId, int>(FieldName)
            .WithConversions(getValue, UMX.tag<todoItemId>)
            .IsPositive()
            .Create()
