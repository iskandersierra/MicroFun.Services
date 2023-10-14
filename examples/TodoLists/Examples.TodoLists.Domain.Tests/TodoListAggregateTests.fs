module TodoListAggregateTests

open System
open Xunit
open FsCheck.Xunit
open Validus

open Examples.TodoLists.Domain

[<Literal>]
let Verbose = false

[<Fact>]
let ``TodoListAggregate.initial should be None`` () =
    let expected = TodoListAggregate.None
    let actual = TodoListAggregate.initial

    Assert.Equal(expected, actual)


[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent Created`` (aggregate: TodoListAggregate) (title: string) =
    let event =
        TodoListEvent.Created(TodoListTitle.UnsafeParse title)

    let expected =
        TodoListAggregate.Existing
            { TodoListState.title = TodoListTitle.UnsafeParse title
              items = []
              nextId = TodoItemId.initial }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent TitleChanged`` (aggregate: TodoListAggregate) (title: string) =
    let event =
        TodoListEvent.TitleChanged(TodoListTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            TodoListAggregate.Existing { state with title = TodoListTitle.UnsafeParse title }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent Archived`` (aggregate: TodoListAggregate) =
    let event = TodoListEvent.Archived

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing _ -> TodoListAggregate.Archived

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemAdded`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let event =
        TodoListEvent.ItemAdded(TodoItemId.unsafeParse itemId, TodoItemTitle.unsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let newItem =
                { TodoItemState.id = TodoItemId.unsafeParse itemId
                  title = TodoItemTitle.unsafeParse title
                  completed = false }

            TodoListAggregate.Existing
                { state with
                    items = newItem :: state.items
                    nextId = TodoItemId.unsafeParse (itemId + 1) }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemTitleChanged`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let event =
        TodoListEvent.ItemTitleChanged(TodoItemId.unsafeParse itemId, TodoItemTitle.unsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.unsafeParse itemId then
                    { item with title = TodoItemTitle.unsafeParse title }
                else
                    item

            TodoListAggregate.Existing { state with items = List.map updateItem state.items }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemCompleted`` (aggregate: TodoListAggregate) (itemId: int) =
    let event =
        TodoListEvent.ItemCompleted(TodoItemId.unsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.unsafeParse itemId then
                    { item with completed = true }
                else
                    item

            TodoListAggregate.Existing { state with items = List.map updateItem state.items }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemReopened`` (aggregate: TodoListAggregate) (itemId: int) =
    let event =
        TodoListEvent.ItemReopened(TodoItemId.unsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.unsafeParse itemId then
                    { item with completed = false }
                else
                    item

            TodoListAggregate.Existing { state with items = List.map updateItem state.items }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemArchived`` (aggregate: TodoListAggregate) (itemId: int) =
    let event =
        TodoListEvent.ItemArchived(TodoItemId.unsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let isItem item = item.id = TodoItemId.unsafeParse itemId

            TodoListAggregate.Existing { state with items = List.filter (not << isItem) state.items }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)


let assertExecuteResult
    (expected: ValidationResult<TodoListEvent list>)
    (actual: ValidationResult<TodoListEvent list>)
    =
    match expected, actual with
    | Ok expected, Ok actual -> Assert.Equal<TodoListEvent>(expected, actual)
    | Error expected, Error actual -> Assert.Equal(expected, actual)
    | Ok expected, Error actual -> Assert.True(false, $"Expected successful {expected}, but was error: {actual}")
    | Error expected, Ok actual -> Assert.True(false, $"Expected error {expected}, but was successful: {actual}")

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand Create`` (aggregate: TodoListAggregate) (title: string) =
    let command =
        TodoListCommand.Create(TodoListTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> Ok [ TodoListEvent.Created(TodoListTitle.UnsafeParse title) ]
        | _ -> Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorAlreadyExists ])

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ChangeTitle`` (aggregate: TodoListAggregate) (title: string) =
    let command =
        TodoListCommand.ChangeTitle(TodoListTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            if state.title = TodoListTitle.UnsafeParse title then
                Ok []
            else
                Ok [ TodoListEvent.TitleChanged(TodoListTitle.UnsafeParse title) ]

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand Archive`` (aggregate: TodoListAggregate) =
    let command = TodoListCommand.Archive

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing _ -> Ok [ TodoListEvent.Archived ]

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand AddItem`` (aggregate: TodoListAggregate) (title: string) =
    let command =
        TodoListCommand.AddItem(TodoItemTitle.unsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            Ok [ TodoListEvent.ItemAdded(state.nextId, TodoItemTitle.unsafeParse title) ]

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ChangeItemTitle`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let command =
        TodoListCommand.ChangeItemTitle(TodoItemId.unsafeParse itemId, TodoItemTitle.unsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items |> List.tryFind (fun item -> item.id = TodoItemId.unsafeParse itemId) with
            | Some item ->
                if item.title = TodoItemTitle.unsafeParse title then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemTitleChanged(TodoItemId.unsafeParse itemId, TodoItemTitle.unsafeParse title) ]
            | None ->
                Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ])

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand CompleteItem`` (aggregate: TodoListAggregate) (itemId: int) =
    let command =
        TodoListCommand.CompleteItem(TodoItemId.unsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items |> List.tryFind (fun item -> item.id = TodoItemId.unsafeParse itemId) with
            | Some item ->
                if item.completed then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemCompleted(TodoItemId.unsafeParse itemId) ]
            | None ->
                Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ])

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ReopenItem`` (aggregate: TodoListAggregate) (itemId: int) =
    let command =
        TodoListCommand.ReopenItem(TodoItemId.unsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items |> List.tryFind (fun item -> item.id = TodoItemId.unsafeParse itemId) with
            | Some item ->
                if item.completed then
                    Ok [ TodoListEvent.ItemReopened(TodoItemId.unsafeParse itemId) ]
                else
                    Ok []
            | None ->
                Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ])

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ArchiveItem`` (aggregate: TodoListAggregate) (itemId: int) =
    let command =
        TodoListCommand.ArchiveItem(TodoItemId.unsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items |> List.tryFind (fun item -> item.id = TodoItemId.unsafeParse itemId) with
            | Some _ ->
                Ok [ TodoListEvent.ItemArchived(TodoItemId.unsafeParse itemId) ]
            | None ->
                Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ])

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual
