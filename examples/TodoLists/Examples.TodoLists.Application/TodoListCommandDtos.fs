namespace Examples.TodoLists.Application

open Examples.TodoLists.Domain

type TodoListCreate = { title: string }
type TodoListChangeTitle = { entityId: string; title: string }
type TodoListArchive = { entityId: string }


type TodoListAddItem = { entityId: string; title: string }

type TodoListChangeItemTitle =
    { entityId: string
      itemId: int
      title: string }

type TodoListCompleteItem = { entityId: string; itemId: int }
type TodoListReopenItem = { entityId: string; itemId: int }
type TodoListArchiveItem = { entityId: string; itemId: int }


[<RequireQualifiedAccess>]
module TodoListCommand =
    let todoListCreate (dto: TodoListCreate) = TodoListCommand.parseCreate dto.title

    let todoListChangeTitle (dto: TodoListChangeTitle) =
        TodoListCommand.parseChangeTitle dto.title

    let todoListArchive (dto: TodoListArchive) = TodoListCommand.parseArchive ()

    let todoListAddItem (dto: TodoListAddItem) = TodoListCommand.parseAddItem dto.title

    let todoListChangeItemTitle (dto: TodoListChangeItemTitle) =
        TodoListCommand.parseChangeItemTitle dto.itemId dto.title

    let todoListCompleteItem (dto: TodoListCompleteItem) =
        TodoListCommand.parseCompleteItem dto.itemId

    let todoListReopenItem (dto: TodoListReopenItem) =
        TodoListCommand.parseReopenItem dto.itemId

    let todoListArchiveItem (dto: TodoListArchiveItem) =
        TodoListCommand.parseArchiveItem dto.itemId
