namespace Examples.TodoLists.Domain

open FsToolkit.ErrorHandling
open MicroFun
open MicroFun.Shared.Domain
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
          nextId = TodoItemId.valueType.UnsafeParse 1 }

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

    let bindNone fn =
        function
        | TodoListAggregate.None -> fn ()
        | aggregate -> aggregate

    let bindExisting fn =
        function
        | TodoListAggregate.Existing state -> fn state
        | aggregate -> aggregate

    let mapExisting fn =
        bindExisting (fn >> TodoListAggregate.Existing)

    let applyEvent (aggregate: TodoListAggregate) =
        function
        | TodoListEvent.Created title ->
            TodoListState.create title
            |> TodoListAggregate.Existing

        | TodoListEvent.TitleChanged title ->
            aggregate
            |> mapExisting (TodoListState.setTitle title)

        | TodoListEvent.Archived ->
            aggregate
            |> bindExisting (konst TodoListAggregate.Archived)


        | TodoListEvent.ItemAdded (itemId, title) ->
            aggregate
            |> mapExisting (
                TodoListState.addItem (TodoItemState.create itemId title)
                >> TodoListState.setNextId (itemId |> TodoItemId.valueType.MapValue((+) 1))
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


    type Projection() =
        member _.InitialItem = initial
        member _.ApplyEvent aggregate event = applyEvent aggregate event

        interface IItemProjection<TodoListAggregate, TodoListEvent> with
            member this.InitialItem = this.InitialItem
            member this.ApplyEvent aggregate event = this.ApplyEvent aggregate event

    let projection = Projection()


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

    let alreadyExistsError =
        Aggregate.createError LangErrorAlreadyExists

    let doesNotExistsError =
        Aggregate.createError LangErrorDoesNotExist

    let isArchivedError =
        Aggregate.createError LangErrorIsArchived

    let itemDoesNotExistsError =
        Aggregate.createError LangErrorItemDoesNotExist

    let ifNone fn =
        function
        | TodoListAggregate.None -> fn ()
        | _ -> alreadyExistsError

    let ifExists fn =
        function
        | TodoListAggregate.Existing state -> fn state
        | TodoListAggregate.None -> doesNotExistsError
        | TodoListAggregate.Archived _ -> isArchivedError

    let ifItemFound itemId fn =
        ifExists (fun state ->
            state.items
            |> List.tryFind (fun item -> item.id = itemId)
            |> function
                | Some item -> fn item
                | None -> itemDoesNotExistsError)

    let executeCommand (state: TodoListAggregate) (command: TodoListCommand) : ValidationResult<TodoListEvent list> =
        validate {
            match command with
            | TodoListCommand.Create title ->
                return!
                    state
                    |> ifNone (fun () -> Ok [ TodoListEvent.Created title ])

            | TodoListCommand.ChangeTitle title ->
                return!
                    state
                    |> ifExists (fun state ->
                        match state.title = title with
                        | true -> Ok []
                        | false -> Ok [ TodoListEvent.TitleChanged title ])

            | TodoListCommand.Archive ->
                return!
                    state
                    |> ifExists (fun _ -> Ok [ TodoListEvent.Archived ])


            | TodoListCommand.AddItem title ->
                return!
                    state
                    |> ifExists (fun state -> Ok [ TodoListEvent.ItemAdded(state.nextId, title) ])

            | TodoListCommand.ChangeItemTitle (itemId, title) ->
                return!
                    state
                    |> ifItemFound itemId (fun item ->
                        match item.title = title with
                        | true -> Ok []
                        | false -> Ok [ TodoListEvent.ItemTitleChanged(itemId, title) ])

            | TodoListCommand.CompleteItem itemId ->
                return!
                    state
                    |> ifItemFound itemId (fun item ->
                        match item.completed with
                        | true -> Ok []
                        | false -> Ok [ TodoListEvent.ItemCompleted itemId ])

            | TodoListCommand.ReopenItem itemId ->
                return!
                    state
                    |> ifItemFound itemId (fun item ->
                        match item.completed with
                        | false -> Ok []
                        | true -> Ok [ TodoListEvent.ItemReopened itemId ])

            | TodoListCommand.ArchiveItem itemId ->
                return!
                    state
                    |> ifItemFound itemId (fun _ -> Ok [ TodoListEvent.ItemArchived itemId ])
        }

    type Aggregate() =
        member this.ExecuteCommand state command = executeCommand state command

        interface IStateAggregate<TodoListAggregate, TodoListCommand, TodoListEvent> with
            member this.ExecuteCommand state command = this.ExecuteCommand state command

    let aggregate = Aggregate()
