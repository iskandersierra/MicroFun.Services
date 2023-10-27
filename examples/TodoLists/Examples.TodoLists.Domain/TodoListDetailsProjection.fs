namespace Examples.TodoLists.Domain

open FsToolkit.ErrorHandling
open MicroFun.Shared.Domain


[<RequireQualifiedAccess>]
type TodoListDetails =
    | None
    | Existing of TodoListDetailsState
    | Archived

and TodoListDetailsState =
    { title: TodoListTitle
      items: TodoItemDetailsState list }

and TodoItemDetailsState =
    { id: TodoItemId
      title: TodoItemTitle
      completed: bool }


[<RequireQualifiedAccess>]
module TodoItemDetailsState =
    let create id title =
        { TodoItemDetailsState.id = id
          title = title
          completed = false }

    let setTitle title (state: TodoItemDetailsState) = { state with title = title }
    let setCompleted completed (state: TodoItemDetailsState) = { state with completed = completed }


[<RequireQualifiedAccess>]
module TodoListDetailsState =
    let create title =
        { TodoListDetailsState.title = title
          items = [] }

    let setTitle title (state: TodoListDetailsState) = { state with title = title }

    let updateItems (fn: TodoItemDetailsState list -> TodoItemDetailsState list) (state: TodoListDetailsState) =
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
module TodoListDetails =
    let initial = TodoListDetails.None

    let bindNone fn =
        function
        | TodoListDetails.None -> fn ()
        | aggregate -> aggregate

    let bindExisting fn =
        function
        | TodoListDetails.Existing state -> fn state
        | aggregate -> aggregate

    let mapExisting fn =
        bindExisting (fn >> TodoListDetails.Existing)


    let applyEvent (aggregate: TodoListDetails) =
        function
        | TodoListEvent.Created title ->
            TodoListDetailsState.create title
            |> TodoListDetails.Existing

        | TodoListEvent.TitleChanged title ->
            aggregate
            |> mapExisting (TodoListDetailsState.setTitle title)

        | TodoListEvent.Archived ->
            match aggregate with
            | TodoListDetails.None -> aggregate
            | TodoListDetails.Archived _ -> aggregate
            | TodoListDetails.Existing _ -> TodoListDetails.Archived


        | TodoListEvent.ItemAdded (itemId, title) ->
            aggregate
            |> mapExisting (TodoListDetailsState.addItem (TodoItemDetailsState.create itemId title))

        | TodoListEvent.ItemTitleChanged (itemId, title) ->
            aggregate
            |> mapExisting (TodoListDetailsState.updateItem itemId (TodoItemDetailsState.setTitle title))

        | TodoListEvent.ItemCompleted itemId ->
            aggregate
            |> mapExisting (TodoListDetailsState.updateItem itemId (TodoItemDetailsState.setCompleted true))

        | TodoListEvent.ItemReopened itemId ->
            aggregate
            |> mapExisting (TodoListDetailsState.updateItem itemId (TodoItemDetailsState.setCompleted false))

        | TodoListEvent.ItemArchived itemId ->
            aggregate
            |> mapExisting (TodoListDetailsState.removeItem itemId)

    type Projection() =
        member this.InitialItem = initial
        member this.ApplyEvent aggregate event = applyEvent aggregate event

        interface IItemProjection<TodoListDetails, TodoListEvent> with
            member this.InitialItem = initial
            member this.ApplyEvent aggregate event = applyEvent aggregate event

    let projection = Projection()
