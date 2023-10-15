namespace Examples.TodoLists.Domain

open Validus


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


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TodoListCommand =
    let parseCreate (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! title = TodoListTitle.TryParse title
            return TodoListCommand.Create title
        }

    let parseChangeTitle (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! title = TodoListTitle.TryParse title
            return TodoListCommand.ChangeTitle title
        }

    let parseArchive () : ValidationResult<TodoListCommand> =
        validate { return TodoListCommand.Archive }


    let parseAddItem (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! title = TodoItemTitle.TryParse title
            return TodoListCommand.AddItem title
        }

    let parseChangeItemTitle (itemId: int) (title: string) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.TryParse itemId
            and! title = TodoItemTitle.TryParse title
            return TodoListCommand.ChangeItemTitle(itemId, title)
        }

    let parseCompleteItem (itemId: int) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.TryParse itemId
            return TodoListCommand.CompleteItem itemId
        }

    let parseReopenItem (itemId: int) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.TryParse itemId
            return TodoListCommand.ReopenItem itemId
        }

    let parseArchiveItem (itemId: int) : ValidationResult<TodoListCommand> =
        validate {
            let! itemId = TodoItemId.TryParse itemId
            return TodoListCommand.ArchiveItem itemId
        }
