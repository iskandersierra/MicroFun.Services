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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TodoListId =
    [<Literal>]
    let Prefix = "tdl-"

    [<Literal>]
    let FieldName = "TodoListId"

    let inline getValue (value: TodoListId) = UMX.untag value
    let inline unsafeParse (value: string) : TodoListId = UMX.tag<todoListId> value

    let validator =
        EntityIdValueType.prefixedValidator Prefix

    let parse =
        EntityIdValueType.parseTrimmed FieldName unsafeParse validator

    let mapValue =
        ValueType.valueMapper getValue unsafeParse


// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// module TodoItemId =
//     [<Literal>]
//     let FieldName = "TodoItemId"

//     let inline getValue (value: TodoItemId) = UMX.untag value
//     let inline unsafeParse (value: int) : TodoItemId = UMX.tag<todoItemId> value

//     let initial = unsafeParse 1

//     let validator = Int32ValueType.mustBePositiveValidator

//     let parse =
//         ValueType.parse FieldName unsafeParse validator

//     let mapValue =
//         ValueType.valueMapper getValue unsafeParse
