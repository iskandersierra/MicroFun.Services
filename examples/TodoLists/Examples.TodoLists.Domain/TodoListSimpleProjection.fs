namespace Examples.TodoLists.Domain


[<RequireQualifiedAccess>]
type TodoListSimpleProjection =
    | None
    | Existing of TodoListSimpleState
    | Archived

and TodoListSimpleState =
    { title: TodoListTitle }


[<RequireQualifiedAccess>]
module TodoListSimpleState =
    let create title =
        { TodoListSimpleState.title = title }

    let setTitle title (state: TodoListSimpleState) = { state with title = title }


[<RequireQualifiedAccess>]
module TodoListSimpleProjection =
    let initial = TodoListSimpleProjection.None

    let mapExisting fn aggregate =
        match aggregate with
        | TodoListSimpleProjection.None -> aggregate
        | TodoListSimpleProjection.Archived _ -> aggregate
        | TodoListSimpleProjection.Existing state -> TodoListSimpleProjection.Existing(fn state)


    let applyEvent (aggregate: TodoListSimpleProjection) =
        function
        | TodoListEvent.Created title ->
            TodoListSimpleState.create title
            |> TodoListSimpleProjection.Existing

        | TodoListEvent.TitleChanged title ->
            aggregate
            |> mapExisting (TodoListSimpleState.setTitle title)

        | TodoListEvent.Archived ->
            match aggregate with
            | TodoListSimpleProjection.None -> aggregate
            | TodoListSimpleProjection.Archived _ -> aggregate
            | TodoListSimpleProjection.Existing _ -> TodoListSimpleProjection.Archived


        | _ -> aggregate
