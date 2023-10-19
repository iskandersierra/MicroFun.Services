module TodoListAggregateTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Validus

open MicroFun
open MicroFun.Shared.Domain.Testing
open Examples.TodoLists.Domain

[<Literal>]
let Verbose = false


type TodoListAggregateArb() =
    static member TodoListTitle() : Arbitrary<TodoListTitle> =
        MicroArb.ValueType.arbitrary
            TodoListTitle.valueType
            MicroGen.Sentence.generateSentenceRaw
            MicroShrink.Sentence.shrinkRaw

    static member TodoItemTitle() : Arbitrary<TodoItemTitle> =
        MicroArb.ValueType.arbitrary
            TodoItemTitle.valueType
            MicroGen.Sentence.generateSentenceRaw
            MicroShrink.Sentence.shrinkRaw

    static member TodoItemId() : Arbitrary<TodoItemId> =
        MicroArb.ValueType.arbitraryFromArb
            TodoItemId.valueType
            (Arb.Default.PositiveInt()
             |> Arb.convert (fun (PositiveInt value) -> value) PositiveInt)

    static member TodoItemState() : Arbitrary<TodoItemState> =
        let mapper id title completed =
            { TodoItemState.id = id
              title = title
              completed = completed }

        let unmapper (entity: TodoItemState) =
            entity.id, entity.title, entity.completed

        MicroArb.Record3.arbitrary
            {| field1 = TodoListAggregateArb.TodoItemId()
               field2 = TodoListAggregateArb.TodoItemTitle()
               field3 = Arb.Default.Bool() |}
            mapper
            unmapper

    static member TodoListState() : Arbitrary<TodoListState> =
        let mapper title items nextId =
            { TodoListState.title = title
              items = items
              nextId = nextId }

        let unmapper (entity: TodoListState) =
            entity.title, entity.items, entity.nextId

        let itemsArb =
            let g =
                Gen.listOf (TodoListAggregateArb.TodoItemState().Generator)

            let s = Arb.Default.FsList().Shrinker
            Arb.fromGenShrink (g, s)

        MicroArb.Record3.arbitrary
            {| field1 = TodoListAggregateArb.TodoListTitle()
               field2 = itemsArb
               field3 = TodoListAggregateArb.TodoItemId() |}
            mapper
            unmapper

    static member TodoListAggregate() : Arbitrary<TodoListAggregate> =
        let generator =
            let none = Gen.constant TodoListAggregate.None
            let archived = Gen.constant TodoListAggregate.Archived

            let existing =
                TodoListAggregateArb.TodoListState().Generator
                |> Gen.map TodoListAggregate.Existing

            Gen.frequency [ 1, none
                            1, archived
                            8, existing ]

        let shrinker =
            function
            | TodoListAggregate.None -> Seq.empty
            | TodoListAggregate.Archived -> Seq.empty
            | TodoListAggregate.Existing state ->
                TodoListAggregateArb
                    .TodoListState()
                    .Shrinker state
                |> Seq.map TodoListAggregate.Existing

        Arb.fromGenShrink (generator, shrinker)


[<Fact>]
let ``TodoListAggregate.initial should be None`` () =
    let expected = TodoListAggregate.None
    let actual = TodoListAggregate.initial

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent Created`` (aggregate: TodoListAggregate) (title: string) =
    let event =
        TodoListEvent.Created(TodoListTitle.valueType.UnsafeParse title)

    let expected =
        TodoListAggregate.Existing
            { TodoListState.title = TodoListTitle.valueType.UnsafeParse title
              items = []
              nextId = TodoItemId.valueType.UnsafeParse 1 }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent TitleChanged`` (aggregate: TodoListAggregate) (title: string) =
    let event =
        TodoListEvent.TitleChanged(TodoListTitle.valueType.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            TodoListAggregate.Existing { state with title = TodoListTitle.valueType.UnsafeParse title }

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
        TodoListEvent.ItemAdded(TodoItemId.valueType.UnsafeParse itemId, TodoItemTitle.valueType.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let newItem =
                { TodoItemState.id = TodoItemId.valueType.UnsafeParse itemId
                  title = TodoItemTitle.valueType.UnsafeParse title
                  completed = false }

            TodoListAggregate.Existing
                { state with
                    items = newItem :: state.items
                    nextId = TodoItemId.valueType.UnsafeParse(itemId + 1) }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemTitleChanged`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let event =
        TodoListEvent.ItemTitleChanged(
            TodoItemId.valueType.UnsafeParse itemId,
            TodoItemTitle.valueType.UnsafeParse title
        )

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.valueType.UnsafeParse itemId then
                    { item with title = TodoItemTitle.valueType.UnsafeParse title }
                else
                    item

            TodoListAggregate.Existing { state with items = List.map updateItem state.items }

    let actual =
        TodoListAggregate.applyEvent aggregate event

    Assert.Equal(expected, actual)

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.applyEvent ItemCompleted`` (aggregate: TodoListAggregate) (itemId: int) =
    let event =
        TodoListEvent.ItemCompleted(TodoItemId.valueType.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.valueType.UnsafeParse itemId then
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
        TodoListEvent.ItemReopened(TodoItemId.valueType.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let updateItem item =
                if item.id = TodoItemId.valueType.UnsafeParse itemId then
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
        TodoListEvent.ItemArchived(TodoItemId.valueType.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> aggregate
        | TodoListAggregate.Archived _ -> aggregate
        | TodoListAggregate.Existing state ->
            let isItem item =
                item.id = TodoItemId.valueType.UnsafeParse itemId

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
        TodoListCommand.Create(TodoListTitle.valueType.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> Ok [ TodoListEvent.Created(TodoListTitle.valueType.UnsafeParse title) ]
        | _ -> Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorAlreadyExists ])

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ChangeTitle`` (aggregate: TodoListAggregate) (title: string) =
    let command =
        TodoListCommand.ChangeTitle(TodoListTitle.valueType.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            if state.title = TodoListTitle.valueType.UnsafeParse title then
                Ok []
            else
                Ok [ TodoListEvent.TitleChanged(TodoListTitle.valueType.UnsafeParse title) ]

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
        TodoListCommand.AddItem(TodoItemTitle.valueType.UnsafeParse title)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            Ok [ TodoListEvent.ItemAdded(state.nextId, TodoItemTitle.valueType.UnsafeParse title) ]

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual

[<Property(Verbose = Verbose)>]
let ``TodoListAggregate.executeCommand ChangeItemTitle`` (aggregate: TodoListAggregate) (itemId: int) (title: string) =
    let command =
        TodoListCommand.ChangeItemTitle(
            TodoItemId.valueType.UnsafeParse itemId,
            TodoItemTitle.valueType.UnsafeParse title
        )

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.valueType.UnsafeParse itemId)
                with
            | Some item ->
                if item.title = TodoItemTitle.valueType.UnsafeParse title then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemTitleChanged(
                             TodoItemId.valueType.UnsafeParse itemId,
                             TodoItemTitle.valueType.UnsafeParse title
                         ) ]
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
        TodoListCommand.CompleteItem(TodoItemId.valueType.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.valueType.UnsafeParse itemId)
                with
            | Some item ->
                if item.completed then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemCompleted(TodoItemId.valueType.UnsafeParse itemId) ]
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
        TodoListCommand.ReopenItem(TodoItemId.valueType.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.valueType.UnsafeParse itemId)
                with
            | Some item ->
                if item.completed then
                    Ok [ TodoListEvent.ItemReopened(TodoItemId.valueType.UnsafeParse itemId) ]
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
        TodoListCommand.ArchiveItem(TodoItemId.valueType.UnsafeParse itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorDoesNotExist ])
        | TodoListAggregate.Archived _ ->
            Error(ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorIsArchived ])
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = TodoItemId.valueType.UnsafeParse itemId)
                with
            | Some _ -> Ok [ TodoListEvent.ItemArchived(TodoItemId.valueType.UnsafeParse itemId) ]
            | None ->
                Error(
                    ValidationErrors.create TodoListAggregate.Aggregate [ TodoListAggregate.LangErrorItemDoesNotExist ]
                )

    let actual =
        TodoListAggregate.executeCommand aggregate command

    assertExecuteResult expected actual
