module Examples.TodoLists.Domain.Tests.TodoListAggregateTests

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
    static member TodoItemState() : Arbitrary<TodoItemState> =
        let mapper id title completed =
            { TodoItemState.id = id
              title = title
              completed = completed }

        let unmapper (entity: TodoItemState) =
            entity.id, entity.title, entity.completed

        MicroArb.Record3.arbitrary
            {| field1 = TodoListValueTypesArb.TodoItemId()
               field2 = TodoListValueTypesArb.TodoItemTitle()
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
            {| field1 = TodoListValueTypesArb.TodoListTitle()
               field2 = itemsArb
               field3 = TodoListValueTypesArb.TodoItemId() |}
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

[<AttributeUsage(AttributeTargets.Method
                 ||| AttributeTargets.Property,
                 AllowMultiple = false)>]
type TodoListAggregatePropertyAttribute() as this =
    inherit PropertyAttribute()

    do
        this.Verbose <- Verbose

        this.Arbitrary <-
            [| typeof<TodoListValueTypesArb>
               typeof<TodoListAggregateArb> |]

let updateItemOnExisting itemId f =
    TodoListState.updateItem itemId f |> TodoListAggregate.mapExisting

let removeItemOnExisting itemId =
    TodoListState.removeItem itemId |> TodoListAggregate.mapExisting


[<Fact>]
let ``TodoListAggregate.initial should be None`` () =
    let expected = TodoListAggregate.None
    let actual = TodoListAggregate.projection.InitialItem

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent Created`` (aggregate: TodoListAggregate) (title: TodoListTitle) =
    let event = TodoListEvent.Created(title)

    let expected =
        TodoListAggregate.Existing
            { TodoListState.title = title
              items = []
              nextId = TodoItemId.valueType.Parse 1 }

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent TitleChanged`` (aggregate: TodoListAggregate) (title: TodoListTitle) =
    let event = TodoListEvent.TitleChanged(title)

    let expected =
        aggregate
        |> TodoListAggregate.mapExisting (TodoListState.setTitle title)

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent Archived`` (aggregate: TodoListAggregate) =
    let event = TodoListEvent.Archived

    let expected =
        aggregate
        |> TodoListAggregate.bindExisting (konst TodoListAggregate.Archived)

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent ItemAdded``
    (aggregate: TodoListAggregate)
    (itemId: TodoItemId)
    (title: TodoItemTitle)
    =
    let event = TodoListEvent.ItemAdded(itemId, title)

    let expected =
        let newItem =
            { TodoItemState.id = itemId
              title = title
              completed = false }

        let nextId = itemId |> TodoItemId.valueType.MapValue ((+) 1)

        aggregate
        |> TodoListAggregate.mapExisting
            (TodoListState.addItem newItem >>
             TodoListState.setNextId nextId)

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent ItemTitleChanged``
    (aggregate: TodoListAggregate)
    (itemId: TodoItemId)
    (title: TodoItemTitle)
    =
    let event =
        TodoListEvent.ItemTitleChanged(itemId, title)

    let expected =
        aggregate
        |> updateItemOnExisting itemId (TodoItemState.setTitle title)

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent ItemCompleted`` (aggregate: TodoListAggregate) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemCompleted(itemId)

    let expected =
        aggregate
        |> updateItemOnExisting itemId (TodoItemState.setCompleted true)

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent ItemReopened`` (aggregate: TodoListAggregate) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemReopened(itemId)

    let expected =
        aggregate
        |> updateItemOnExisting itemId (TodoItemState.setCompleted false)

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListAggregateProperty>]
let ``TodoListAggregate.applyEvent ItemArchived`` (aggregate: TodoListAggregate) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemArchived(itemId)

    let expected =
        aggregate
        |> removeItemOnExisting itemId

    let actual =
        TodoListAggregate.projection.ApplyEvent aggregate event

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

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand Create`` (aggregate: TodoListAggregate) (title: TodoListTitle) =
    let command = TodoListCommand.Create(title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> Ok [ TodoListEvent.Created(title) ]
        | _ -> TodoListAggregate.alreadyExistsError

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand ChangeTitle`` (aggregate: TodoListAggregate) (title: TodoListTitle) =
    let command = TodoListCommand.ChangeTitle(title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ -> TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing state ->
            if state.title = title then
                Ok []
            else
                Ok [ TodoListEvent.TitleChanged(title) ]

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand Archive`` (aggregate: TodoListAggregate) =
    let command = TodoListCommand.Archive

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ ->TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing _ -> Ok [ TodoListEvent.Archived ]

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand AddItem`` (aggregate: TodoListAggregate) (title: TodoItemTitle) =
    let command = TodoListCommand.AddItem(title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ -> TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing state -> Ok [ TodoListEvent.ItemAdded(state.nextId, title) ]

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand ChangeItemTitle`` (aggregate: TodoListAggregate) (itemId: TodoItemId) (title: TodoItemTitle) =
    let command =
        TodoListCommand.ChangeItemTitle(itemId, title)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ -> TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = itemId)
                with
            | Some item ->
                if item.title = title then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemTitleChanged(itemId, title) ]
            | None -> TodoListAggregate.itemDoesNotExistsError

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand CompleteItem`` (aggregate: TodoListAggregate) (itemId: TodoItemId) =
    let command = TodoListCommand.CompleteItem(itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ -> TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = itemId)
                with
            | Some item ->
                if item.completed then
                    Ok []
                else
                    Ok [ TodoListEvent.ItemCompleted(itemId) ]
            | None -> TodoListAggregate.itemDoesNotExistsError

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand ReopenItem`` (aggregate: TodoListAggregate) (itemId: TodoItemId) =
    let command = TodoListCommand.ReopenItem(itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ -> TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = itemId)
                with
            | Some item ->
                if item.completed then
                    Ok [ TodoListEvent.ItemReopened(itemId) ]
                else
                    Ok []
            | None -> TodoListAggregate.itemDoesNotExistsError

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual

[<TodoListAggregateProperty>]
let ``TodoListAggregate.executeCommand ArchiveItem`` (aggregate: TodoListAggregate) (itemId: TodoItemId) =
    let command = TodoListCommand.ArchiveItem(itemId)

    let expected =
        match aggregate with
        | TodoListAggregate.None -> TodoListAggregate.doesNotExistsError
        | TodoListAggregate.Archived _ -> TodoListAggregate.isArchivedError
        | TodoListAggregate.Existing state ->
            match state.items
                  |> List.tryFind (fun item -> item.id = itemId)
                with
            | Some _ -> Ok [ TodoListEvent.ItemArchived(itemId) ]
            | None -> TodoListAggregate.itemDoesNotExistsError

    let actual =
        TodoListAggregate.aggregate.ExecuteCommand aggregate command

    assertExecuteResult expected actual
