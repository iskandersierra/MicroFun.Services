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

    let bindNone fn =
        function
        | TodoListSimple.None -> fn ()
        | aggregate -> aggregate

    let bindExisting fn =
        function
        | TodoListSimple.Existing state -> fn state
        | aggregate -> aggregate

    let mapExisting fn =
        bindExisting (fn >> TodoListSimple.Existing)


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

    type Projection() =
        member this.InitialItem = initial
        member this.ApplyEvent aggregate event = applyEvent aggregate event

        interface IItemProjection<TodoListSimple, TodoListEvent> with
            member this.InitialItem = initial
            member this.ApplyEvent aggregate event = applyEvent aggregate event

    let projection = Projection()
