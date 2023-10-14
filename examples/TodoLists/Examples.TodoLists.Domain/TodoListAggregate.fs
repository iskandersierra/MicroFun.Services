namespace Examples.TodoLists.Domain

open FsToolkit.ErrorHandling
open Validus


[<RequireQualifiedAccess>]
type TodoListAggregate =
    | None
    | Existing of TodoListState
    | Archived

and TodoListState =
    { title: TodoListTitle
      items: TodoItemState list
      nextId: TodoItemId }

and TodoItemState =
    { id: TodoItemId
      title: TodoItemTitle
      completed: bool }


[<RequireQualifiedAccess>]
module TodoItemState =
    let create id title =
        { TodoItemState.id = id
          title = title
          completed = false }

    let setTitle title (state: TodoItemState) = { state with title = title }
    let setCompleted completed (state: TodoItemState) = { state with completed = completed }


[<RequireQualifiedAccess>]
module TodoListState =
    let create title =
        { TodoListState.title = title
          items = []
          nextId = TodoItemId.initial }

    let setTitle title (state: TodoListState) = { state with title = title }
    let setNextId nextId (state: TodoListState) = { state with nextId = nextId }

    let updateItems (fn: TodoItemState list -> TodoItemState list) (state: TodoListState) =
        { state with items = fn state.items }

    let addItem item =
        updateItems (fun items -> item :: items)

    let updateItem itemId fn =
        updateItems (fun items ->
            items
            |> List.map (fun item ->
                match item.id = itemId with
                | false -> item
                | true -> fn item))

    let removeItem itemId =
        updateItems (fun items ->
            items
            |> List.filter (fun item -> item.id <> itemId))


[<RequireQualifiedAccess>]
module TodoListAggregate =
    let initial = TodoListAggregate.None

    let mapExisting fn aggregate =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state -> TodoListAggregate.Existing(fn state)


    let applyEvent (aggregate: TodoListAggregate) =
        function
        | TodoListEvent.Created title ->
            TodoListState.create title
            |> TodoListAggregate.Existing

        | TodoListEvent.TitleChanged title ->
            aggregate
            |> mapExisting (TodoListState.setTitle title)

        | TodoListEvent.Archived ->
            match aggregate with
            | TodoListAggregate.None -> aggregate
            | TodoListAggregate.Archived _ -> aggregate
            | TodoListAggregate.Existing _ -> TodoListAggregate.Archived


        | TodoListEvent.ItemAdded (itemId, title) ->
            aggregate
            |> mapExisting (
                TodoListState.addItem (TodoItemState.create itemId title)
                >> TodoListState.setNextId (itemId |> TodoItemId.mapValue ((+) 1))
            )

        | TodoListEvent.ItemTitleChanged (itemId, title) ->
            aggregate
            |> mapExisting (TodoListState.updateItem itemId (TodoItemState.setTitle title))

        | TodoListEvent.ItemCompleted itemId ->
            aggregate
            |> mapExisting (TodoListState.updateItem itemId (TodoItemState.setCompleted true))

        | TodoListEvent.ItemReopened itemId ->
            aggregate
            |> mapExisting (TodoListState.updateItem itemId (TodoItemState.setCompleted false))

        | TodoListEvent.ItemArchived itemId ->
            aggregate
            |> mapExisting (TodoListState.removeItem itemId)

    [<Literal>]
    let Aggregate = "Aggregate"

    [<Literal>]
    let LangErrorAlreadyExists =
        "lang:Examples.TodoList.TodoListAggregate.Error.AlreadyExists"

    [<Literal>]
    let LangErrorDoesNotExist =
        "lang:Examples.TodoList.TodoListAggregate.Error.DoesNotExist"

    [<Literal>]
    let LangErrorItemDoesNotExist =
        "lang:Examples.TodoList.TodoListAggregate.Error.ItemDoesNotExist"

    [<Literal>]
    let LangErrorIsArchived =
        "lang:Examples.TodoList.TodoListAggregate.Error.IsArchived"

    let executeCommand
        (aggregate: TodoListAggregate)
        (command: TodoListCommand)
        : ValidationResult<TodoListEvent list> =
        let ifNone fn =
            match aggregate with
            | TodoListAggregate.None -> fn ()
            | _ -> Error(ValidationErrors.create Aggregate [ LangErrorAlreadyExists ])

        let ifExists fn =
            match aggregate with
            | TodoListAggregate.Existing state -> fn state
            | TodoListAggregate.None -> Error(ValidationErrors.create Aggregate [ LangErrorDoesNotExist ])
            | TodoListAggregate.Archived _ -> Error(ValidationErrors.create Aggregate [ LangErrorIsArchived ])

        let ifItemFound itemId fn =
            ifExists (fun state ->
                match state.items
                      |> List.tryFind (fun item -> item.id = itemId)
                    with
                | Some item -> fn item
                | None -> Error(ValidationErrors.create Aggregate [ LangErrorItemDoesNotExist ]))


        validate {
            match command with
            | TodoListCommand.Create title -> return! ifNone (fun () -> Ok [ TodoListEvent.Created title ])

            | TodoListCommand.ChangeTitle title ->
                return!
                    ifExists (fun state ->
                        match state.title = title with
                        | true -> Ok []
                        | false -> Ok [ TodoListEvent.TitleChanged title ])

            | TodoListCommand.Archive -> return! ifExists (fun _ -> Ok [ TodoListEvent.Archived ])


            | TodoListCommand.AddItem title ->
                return! ifExists (fun state -> Ok [ TodoListEvent.ItemAdded(state.nextId, title) ])

            | TodoListCommand.ChangeItemTitle (itemId, title) ->
                return!
                    ifItemFound itemId (fun item ->
                        match item.title = title with
                        | true -> Ok []
                        | false -> Ok [ TodoListEvent.ItemTitleChanged(itemId, title) ])

            | TodoListCommand.CompleteItem itemId ->
                return!
                    ifItemFound itemId (fun item ->
                        match item.completed with
                        | true -> Ok []
                        | false -> Ok [ TodoListEvent.ItemCompleted itemId ])

            | TodoListCommand.ReopenItem itemId ->
                return!
                    ifItemFound itemId (fun item ->
                        match item.completed with
                        | false -> Ok []
                        | true -> Ok [ TodoListEvent.ItemReopened itemId ])

            | TodoListCommand.ArchiveItem itemId ->
                return! ifItemFound itemId (fun _ -> Ok [ TodoListEvent.ItemArchived itemId ])
        }
