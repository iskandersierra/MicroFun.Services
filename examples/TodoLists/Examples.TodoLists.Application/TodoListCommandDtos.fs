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
module TodoListCreate =
    let toCommand (dto: TodoListCreate) =
        TodoListCommand.parseCreate dto.title

[<RequireQualifiedAccess>]
module TodoListChangeTitle =
    let toCommand (dto: TodoListChangeTitle) =
        TodoListCommand.parseChangeTitle dto.title

[<RequireQualifiedAccess>]
module TodoListArchive =
    let toCommand (dto: TodoListArchive) =
        TodoListCommand.parseArchive ()

[<RequireQualifiedAccess>]
module TodoListAddItem =
    let toCommand (dto: TodoListAddItem) =
        TodoListCommand.parseAddItem dto.title

[<RequireQualifiedAccess>]
module TodoListChangeItemTitle =
    let toCommand (dto: TodoListChangeItemTitle) =
        TodoListCommand.parseChangeItemTitle dto.itemId dto.title

[<RequireQualifiedAccess>]
module TodoListCompleteItem =
    let toCommand (dto: TodoListCompleteItem) =
        TodoListCommand.parseCompleteItem dto.itemId

[<RequireQualifiedAccess>]
module TodoListReopenItem =
    let toCommand (dto: TodoListReopenItem) =
        TodoListCommand.parseReopenItem dto.itemId

[<RequireQualifiedAccess>]
module TodoListArchiveItem =
    let toCommand (dto: TodoListArchiveItem) =
        TodoListCommand.parseArchiveItem dto.itemId
