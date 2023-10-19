namespace Examples.TodoLists.Domain

open MicroFun.Shared.Domain


[<RequireQualifiedAccess>]
type TodoListSimple =
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
module TodoListSimple =
    let initial = TodoListSimple.None

    let mapExisting fn aggregate =
        match aggregate with
        | TodoListSimple.None -> aggregate
        | TodoListSimple.Archived _ -> aggregate
        | TodoListSimple.Existing state -> TodoListSimple.Existing(fn state)


    let applyEvent (aggregate: TodoListSimple) =
        function
        | TodoListEvent.Created title ->
            TodoListSimpleState.create title
            |> TodoListSimple.Existing

        | TodoListEvent.TitleChanged title ->
            aggregate
            |> mapExisting (TodoListSimpleState.setTitle title)

        | TodoListEvent.Archived ->
            match aggregate with
            | TodoListSimple.None -> aggregate
            | TodoListSimple.Archived _ -> aggregate
            | TodoListSimple.Existing _ -> TodoListSimple.Archived


        | _ -> aggregate

    let projection =
        { new IItemProjection<TodoListSimple, TodoListEvent> with
              member this.InitialItem = initial
              member this.ApplyEvent aggregate event = applyEvent aggregate event }
