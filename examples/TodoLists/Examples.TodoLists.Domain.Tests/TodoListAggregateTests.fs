module TodoListAggregateTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Validus

open MicroFun
open Examples.TodoLists.Domain

[<Literal>]
let Verbose = false

let internal getEnumeratorStatusOr previousValue enumerator =
    enumerator
    |> Seq.tryMoveNext
    |> function
        | None -> previousValue, false
        | Some value -> value, true

let shrinkFields2 field1 field2 field1Shrinker field2Shrinker =
    seq {
        use enumerator1 = field1Shrinker |> Seq.getEnumerator
        use enumerator2 = field2Shrinker |> Seq.getEnumerator

        let rec loop index (v1, a1) (v2, a2) =
            seq {
                match index with
                | 0 ->
                    if a1 then
                        yield v1, v2
                        let status1 = getEnumeratorStatusOr v1 enumerator1
                        yield! loop 1 status1 (v2, a2)
                    else
                        yield! loop 1 (v1, a1) (v2, a2)

                | 1 ->
                    if a2 then
                        yield v1, v2
                        let status2 = getEnumeratorStatusOr v2 enumerator2
                        yield! loop 2 (v1, a1) status2
                    else
                        yield! loop 2 (v1, a1) (v2, a2)

                | _ ->
                    match a1, a2 with
                    | false, false -> ()
                    | _ -> yield! loop 0 (v1, a1) (v2, a2)
            }

        let status1 = getEnumeratorStatusOr field1 enumerator1
        let status2 = getEnumeratorStatusOr field2 enumerator2

        yield! loop 0 status1 status2
    }

let shrinkFields3 field1 field2 field3 field1Shrinker field2Shrinker field3Shrinker =
    seq {
        use enumerator1 = field1Shrinker |> Seq.getEnumerator
        use enumerator2 = field2Shrinker |> Seq.getEnumerator
        use enumerator3 = field3Shrinker |> Seq.getEnumerator

        let rec loop index (v1, a1) (v2, a2) (v3, a3) =
            seq {
                match index with
                | 0 ->
                    if a1 then
                        yield v1, v2, v3
                        let status1 = getEnumeratorStatusOr v1 enumerator1
                        yield! loop 1 status1 (v2, a2) (v3, a3)
                    else
                        yield! loop 1 (v1, a1) (v2, a2) (v3, a3)

                | 1 ->
                    if a2 then
                        yield v1, v2, v3
                        let status2 = getEnumeratorStatusOr v2 enumerator2
                        yield! loop 2 (v1, a1) status2 (v3, a3)
                    else
                        yield! loop 2 (v1, a1) (v2, a2) (v3, a3)

                | 2 ->
                    if a3 then
                        yield v1, v2, v3
                        let status3 = getEnumeratorStatusOr v3 enumerator3
                        yield! loop 3 (v1, a1) (v2, a2) status3
                    else
                        yield! loop 3 (v1, a1) (v2, a2) (v3, a3)

                | _ ->
                    match a1, a2, a3 with
                    | false, false, false -> ()
                    | _ -> yield! loop 0 (v1, a1) (v2, a2) (v3, a3)
            }

        let status1 = getEnumeratorStatusOr field1 enumerator1
        let status2 = getEnumeratorStatusOr field2 enumerator2
        let status3 = getEnumeratorStatusOr field3 enumerator3

        yield! loop 0 status1 status2 status3
    }


type TodoListAggregateArb() =
    static member TodoListTitle() : Arbitrary<TodoListTitle> =
        let generator =
            gen {
                let! length = Gen.choose (TodoListTitleType.MinLength, TodoListTitleType.MaxLength)
                let! text =
                    Gen.elements ['a' .. 'z']
                    |> Gen.arrayOfLength length
                    |> Gen.map string
                return TodoListTitle.UnsafeParse text
            }

        let shrinker (value: TodoListTitle) =
            TodoListTitle.GetValue value
            |> Arb.Default.String().Shrinker
            |> Seq.filter (fun text -> TodoListTitle.TryParse text |> Result.isOk)
            |> Seq.map (fun text -> TodoListTitle.UnsafeParse text)

        Arb.fromGenShrink (generator, shrinker)

    static member TodoItemTitle() : Arbitrary<TodoItemTitle> =
        let generator =
            gen {
                let! length = Gen.choose (TodoItemTitleType.MinLength, TodoItemTitleType.MaxLength)
                let! text =
                    Gen.elements ['a' .. 'z']
                    |> Gen.arrayOfLength length
                    |> Gen.map string
                return TodoItemTitle.UnsafeParse text
            }

        let shrinker (value: TodoItemTitle) =
            TodoItemTitle.GetValue value
            |> Arb.Default.String().Shrinker
            |> Seq.filter (fun text -> TodoItemTitle.TryParse text |> Result.isOk)
            |> Seq.map (fun text -> TodoItemTitle.UnsafeParse text)

        Arb.fromGenShrink (generator, shrinker)

    static member TodoItemId() : Arbitrary<TodoItemId> =
        let generator =
            gen {
                let! value = Gen.choose (1, 1000)
                return TodoItemId.UnsafeParse value
            }

        let shrinker (value: TodoItemId) =
            TodoItemId.GetValue value
            |> PositiveInt
            |> Arb.Default.PositiveInt().Shrinker
            |> Seq.map (fun (PositiveInt value) -> TodoItemId.UnsafeParse value)

        Arb.fromGenShrink (generator, shrinker)

    static member TodoItemState() : Arbitrary<TodoItemState> =
        let generator =
            gen {
                let! id = TodoListAggregateArb.TodoItemId().Generator
                let! title = TodoListAggregateArb.TodoItemTitle().Generator
                let! completed = Arb.Default.Bool().Generator
                return { TodoItemState.id = id; title = title; completed = completed }
            }

        let shrinker (value: TodoItemState) =
            seq {
                let idShrinker =
                    TodoListAggregateArb
                        .TodoItemId()
                        .Shrinker value.id

                let titleShrinker =
                    TodoListAggregateArb
                        .TodoItemTitle()
                        .Shrinker value.title

                let completedShrinker =
                    Arb.Default.Bool().Shrinker value.completed

                yield!
                    shrinkFields3 value.id value.title value.completed idShrinker titleShrinker completedShrinker
                    |> Seq.map (fun (id, title, completed) -> { TodoItemState.id = id; title = title; completed = completed })
            }

        Arb.fromGenShrink (generator, shrinker)

    static member TodoListState() : Arbitrary<TodoListState> =
        let generator =
            gen {
                let! title = TodoListAggregateArb.TodoListTitle().Generator

                let! items =
                    Gen.arrayOf (TodoListAggregateArb.TodoItemState().Generator)
                    |> Gen.map List.ofArray

                let! nextId = TodoListAggregateArb.TodoItemId().Generator

                return
                    { TodoListState.title = title
                      items = items
                      nextId = nextId }
            }

        let shrinker (value: TodoListState) =
            seq {
                let titleShrinker =
                    TodoListAggregateArb
                        .TodoListTitle()
                        .Shrinker value.title

                let itemsShrinker =
                    value.items
                    |> List.toArray
                    |> Arb.Default.Array().Shrinker
                    |> Seq.map (fun items -> List.ofArray items)

                let nextIdShrinker =
                    TodoItemId.GetValue value.nextId
                    |> PositiveInt
                    |> Arb.Default.PositiveInt().Shrinker
                    |> Seq.map (fun (PositiveInt value) -> TodoItemId.UnsafeParse value)

                yield!
                    shrinkFields3 value.title value.items value.nextId titleShrinker itemsShrinker nextIdShrinker
                    |> Seq.map (fun (title, items, nextId) ->
                        { TodoListState.title = title
                          items = items
                          nextId = nextId })
            }

        Arb.fromGenShrink (generator, shrinker)

    static member TodoListAggregate() : Arbitrary<TodoListAggregate> =
        let generator =
            Gen.frequency [ 1, Gen.constant TodoListAggregate.None
                            1, Gen.constant TodoListAggregate.Archived
                            8,
                            TodoListAggregateArb.TodoListState().Generator
                            |> Gen.map TodoListAggregate.Existing ]

        let shrinker =
            function
            | TodoListAggregate.None -> Seq.empty
            | TodoListAggregate.Archived -> Seq.empty
            | TodoListAggregate.Existing state ->
                TodoListAggregateArb
                    .TodoListState()
                    .Shrinker state
                |> Seq.map (fun state -> TodoListAggregate.Existing state)

        Arb.fromGenShrink (generator, shrinker)


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
              nextId = TodoItemId.UnsafeParse 1 }

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

[<Property(Verbose = Verbose, Arbitrary = [| typeof<TodoListAggregateArb> |])>]
let ``TodoListAggregate.applyEvent ItemAdded`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let event =
        TodoListEvent.ItemAdded(TodoItemId.UnsafeParse itemId, TodoItemTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let newItem =
                { TodoItemState.id = TodoItemId.UnsafeParse itemId
                  title = TodoItemTitle.UnsafeParse title
                  completed = false }

            TodoListAggregate.Existing
                { state with
                    items = newItem :: state.items
                    nextId = TodoItemId.UnsafeParse(itemId + 1) }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemTitleChanged`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let event =
        TodoListEvent.ItemTitleChanged(TodoItemId.UnsafeParse itemId, TodoItemTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.UnsafeParse itemId then
                    { item with title = TodoItemTitle.UnsafeParse title }
                else
                    item

            TodoListAggregate.Existing { state with items = List.map updateItem state.items }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemCompleted`` (aggregate: TodoListAggregate) (itemId: int) =
    let event =
        TodoListEvent.ItemCompleted(TodoItemId.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.UnsafeParse itemId then
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
        TodoListEvent.ItemReopened(TodoItemId.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.UnsafeParse itemId then
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
        TodoListEvent.ItemArchived(TodoItemId.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let isItem item = item.id = TodoItemId.UnsafeParse itemId

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
        TodoListCommand.AddItem(TodoItemTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            Ok [ TodoListEvent.ItemAdded(state.nextId, TodoItemTitle.UnsafeParse title) ]

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ChangeItemTitle`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let command =
        TodoListCommand.ChangeItemTitle(TodoItemId.UnsafeParse itemId, TodoItemTitle.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.UnsafeParse itemId)
                with
            | Some item ->
                if item.title = TodoItemTitle.UnsafeParse title then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemTitleChanged(TodoItemId.UnsafeParse itemId, TodoItemTitle.UnsafeParse title) ]
            | None ->
                Error(
                    ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ]
                )

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand CompleteItem`` (aggregate: TodoListAggregate) (itemId: int) =
    let command =
        TodoListCommand.CompleteItem(TodoItemId.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.UnsafeParse itemId)
                with
            | Some item ->
                if item.completed then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemCompleted(TodoItemId.UnsafeParse itemId) ]
            | None ->
                Error(
                    ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ]
                )

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ReopenItem`` (aggregate: TodoListAggregate) (itemId: int) =
    let command =
        TodoListCommand.ReopenItem(TodoItemId.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.UnsafeParse itemId)
                with
            | Some item ->
                if item.completed then
                    Ok [ TodoListEvent.ItemReopened(TodoItemId.UnsafeParse itemId) ]
                else
                    Ok []
            | None ->
                Error(
                    ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ]
                )

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ArchiveItem`` (aggregate: TodoListAggregate) (itemId: int) =
    let command =
        TodoListCommand.ArchiveItem(TodoItemId.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.UnsafeParse itemId)
                with
            | Some _ -> Ok [ TodoListEvent.ItemArchived(TodoItemId.UnsafeParse itemId) ]
            | None ->
                Error(
                    ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ]
                )

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual
