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

    type ValueType() =
        inherit CustomValueType<TodoListId, string>(FieldName, fun builder ->
            let getValue (value: TodoListId) = UMX.untag value
            ValueType
                .Builder<TodoListId, string>(FieldName)
                .WithConversions(getValue, UMX.tag<todoListId>)
                .EnsureTrimming()
                .IsEntityId(Prefix))


    let valueType = ValueType()


[<RequireQualifiedAccess>]
module TodoListTitle =
    [<Literal>]
    let FieldName = "TodoListTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

    type ValueType() =
        inherit CustomValueType<TodoListTitle, string>(FieldName, fun builder ->
            let getValue (value: TodoListTitle) = UMX.untag value
            ValueType
                .Builder<TodoListTitle, string>(FieldName)
                .WithConversions(getValue, UMX.tag<todoListTitle>)
                .EnsureTrimming()
                .MustHaveLengthBetween(MinLength, MaxLength))

    let valueType = ValueType()


[<RequireQualifiedAccess>]
module TodoItemTitle =
    [<Literal>]
    let FieldName = "TodoItemTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

    type ValueType() =
        inherit CustomValueType<TodoItemTitle, string>(FieldName, fun builder ->
            let getValue (value: TodoItemTitle) = UMX.untag value
            ValueType
                .Builder<TodoItemTitle, string>(FieldName)
                .WithConversions(getValue, UMX.tag<todoItemTitle>)
                .EnsureTrimming()
                .MustHaveLengthBetween(MinLength, MaxLength))

    let valueType = ValueType()


[<RequireQualifiedAccess>]
module TodoItemId =
    [<Literal>]
    let FieldName = "TodoItemId"

    type ValueType() =
        inherit CustomValueType<TodoItemId, int>(FieldName, fun builder ->
            let getValue (value: TodoItemId) = UMX.untag value
            ValueType
                .Builder<TodoItemId, int>(FieldName)
                .WithConversions(getValue, UMX.tag<todoItemId>)
                .IsPositive())

    let valueType = ValueType()
