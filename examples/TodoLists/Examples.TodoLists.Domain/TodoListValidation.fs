namespace Examples.TodoLists.Domain

open FSharp.UMX
open MicroFun.Shared.Domain
open Validus

module TodoListTitleType =
    [<Literal>]
    let FieldName = "TodoListTitle"
    [<Literal>]
    let MinLength = 3
    [<Literal>]
    let MaxLength = 100

[<AutoOpen>]
module TodoListValueTypes =
    let TodoListTitle =
        let getValue (value: TodoListTitle) = UMX.untag value
        StringValueType
            .Builder<TodoListTitle>(TodoListTitleType.FieldName)
            .WithConversions(getValue, UMX.tag<todoListTitle>)
            .WithLengthBetween(TodoListTitleType.MinLength, TodoListTitleType.MaxLength)
            .WithTrimming()
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
// module TodoListTitle =
//     [<Literal>]
//     let FieldName = "TodoListTitle"

//     [<Literal>]
//     let MinLength = 3

//     [<Literal>]
//     let MaxLength = 100

//     let inline getValue (value: TodoListTitle) = UMX.untag value
//     let inline unsafeParse (value: string) : TodoListTitle = UMX.tag<todoListTitle> value

//     let validator =
//         StringValueType.mustHaveProperLengthValidator MinLength MaxLength

//     let parse =
//         StringValueType.parseTrimmed FieldName unsafeParse validator

//     let mapValue =
//         ValueType.valueMapper getValue unsafeParse


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TodoItemTitle =
    [<Literal>]
    let FieldName = "TodoItemTitle"

    [<Literal>]
    let MinLength = 3

    [<Literal>]
    let MaxLength = 100

    let inline getValue (value: TodoItemTitle) = UMX.untag value
    let inline unsafeParse (value: string) : TodoItemTitle = UMX.tag<todoItemTitle> value

    let validator =
        StringValueType.mustHaveProperLengthValidator MinLength MaxLength

    let parse =
        StringValueType.parseTrimmed FieldName unsafeParse validator

    let mapValue =
        ValueType.valueMapper getValue unsafeParse


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TodoItemId =
    [<Literal>]
    let FieldName = "TodoItemId"

    let inline getValue (value: TodoItemId) = UMX.untag value
    let inline unsafeParse (value: int) : TodoItemId = UMX.tag<todoItemId> value

    let initial = unsafeParse 1

    let validator = Int32ValueType.mustBePositiveValidator

    let parse =
        ValueType.parse FieldName unsafeParse validator

    let mapValue =
        ValueType.valueMapper getValue unsafeParse


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TodoListCommand =
    let parseCreate (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! title = TodoListTitle.Parse title
            return TodoListCommand.Create title
        }

    let parseChangeTitle (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! title = TodoListTitle.Parse title
            return TodoListCommand.ChangeTitle title
        }

    let parseArchive () : ValidationResult<TodoListCommand> =
        validate { return TodoListCommand.Archive }


    let parseAddItem (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! title = TodoItemTitle.parse title
            return TodoListCommand.AddItem title
        }

    let parseChangeItemTitle (itemId: int) (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.parse itemId
            and! title = TodoItemTitle.parse title
            return TodoListCommand.ChangeItemTitle(itemId, title)
        }

    let parseCompleteItem (itemId: int) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.parse itemId
            return TodoListCommand.CompleteItem itemId
        }

    let parseReopenItem (itemId: int) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.parse itemId
            return TodoListCommand.ReopenItem itemId
        }

    let parseArchiveItem (itemId: int) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.parse itemId
            return TodoListCommand.ArchiveItem itemId
        }
