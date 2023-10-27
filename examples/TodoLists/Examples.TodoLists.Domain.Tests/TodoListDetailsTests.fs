module Examples.TodoLists.Domain.Tests.TodoListDetailsTests

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


type TodoListDetailsArb() =
    static member TodoItemDetailsState() : Arbitrary<TodoItemDetailsState> =
        let mapper id title completed =
            { TodoItemDetailsState.id = id
              title = title
              completed = completed }

        let unmapper (entity: TodoItemDetailsState) =
            entity.id, entity.title, entity.completed

        MicroArb.Record3.arbitrary
            {| field1 = TodoListValueTypesArb.TodoItemId()
               field2 = TodoListValueTypesArb.TodoItemTitle()
               field3 = Arb.Default.Bool() |}
            mapper
            unmapper

    static member TodoListDetailsState() : Arbitrary<TodoListDetailsState> =
        let mapper title items =
            { TodoListDetailsState.title = title
              items = items }

        let unmapper (entity: TodoListDetailsState) =
            entity.title, entity.items

        let itemsArb =
            let g =
                Gen.listOf (TodoListDetailsArb.TodoItemDetailsState().Generator)

            let s = Arb.Default.FsList().Shrinker
            Arb.fromGenShrink (g, s)

        MicroArb.Record2.arbitrary
            {| field1 = TodoListValueTypesArb.TodoListTitle()
               field2 = itemsArb |}
            mapper
            unmapper

    static member TodoListDetails() : Arbitrary<TodoListDetails> =
        let generator =
            let none = Gen.constant TodoListDetails.None
            let archived = Gen.constant TodoListDetails.Archived

            let existing =
                TodoListDetailsArb.TodoListDetailsState().Generator
                |> Gen.map TodoListDetails.Existing

            Gen.frequency [ 1, none
                            1, archived
                            8, existing ]

        let shrinker =
            function
            | TodoListDetails.None -> Seq.empty
            | TodoListDetails.Archived -> Seq.empty
            | TodoListDetails.Existing state ->
                TodoListDetailsArb
                    .TodoListDetailsState()
                    .Shrinker state
                |> Seq.map TodoListDetails.Existing

        Arb.fromGenShrink (generator, shrinker)

[<AttributeUsage(AttributeTargets.Method
                 ||| AttributeTargets.Property,
                 AllowMultiple = false)>]
type TodoListDetailsPropertyAttribute() as this =
    inherit PropertyAttribute()

    do
        this.Verbose <- Verbose

        this.Arbitrary <-
            [| typeof<TodoListValueTypesArb>
               typeof<TodoListDetailsArb> |]

let updateItemOnExisting itemId f =
    TodoListDetailsState.updateItem itemId f |> TodoListDetails.mapExisting

let removeItemOnExisting itemId =
    TodoListDetailsState.removeItem itemId |> TodoListDetails.mapExisting


[<Fact>]
let ``TodoListDetails.initial should be None`` () =
    let expected = TodoListDetails.None
    let actual = TodoListDetails.projection.InitialItem

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent Created`` (aggregate: TodoListDetails) (title: TodoListTitle) =
    let event = TodoListEvent.Created(title)

    let expected =
        TodoListDetails.Existing
            { TodoListDetailsState.title = title
              items = [] }

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent TitleChanged`` (aggregate: TodoListDetails) (title: TodoListTitle) =
    let event = TodoListEvent.TitleChanged(title)

    let expected =
        aggregate
        |> TodoListDetails.mapExisting (TodoListDetailsState.setTitle title)

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent Archived`` (aggregate: TodoListDetails) =
    let event = TodoListEvent.Archived

    let expected =
        aggregate
        |> TodoListDetails.bindExisting (konst TodoListDetails.Archived)

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent ItemAdded``
    (aggregate: TodoListDetails)
    (itemId: TodoItemId)
    (title: TodoItemTitle)
    =
    let event = TodoListEvent.ItemAdded(itemId, title)

    let expected =
        let newItem =
            { TodoItemDetailsState.id = itemId
              title = title
              completed = false }

        aggregate
        |> TodoListDetails.mapExisting
            (TodoListDetailsState.addItem newItem)

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent ItemTitleChanged``
    (aggregate: TodoListDetails)
    (itemId: TodoItemId)
    (title: TodoItemTitle)
    =
    let event =
        TodoListEvent.ItemTitleChanged(itemId, title)

    let expected =
        aggregate
        |> updateItemOnExisting itemId (TodoItemDetailsState.setTitle title)

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent ItemCompleted`` (aggregate: TodoListDetails) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemCompleted(itemId)

    let expected =
        aggregate
        |> updateItemOnExisting itemId (TodoItemDetailsState.setCompleted true)

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent ItemReopened`` (aggregate: TodoListDetails) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemReopened(itemId)

    let expected =
        aggregate
        |> updateItemOnExisting itemId (TodoItemDetailsState.setCompleted false)

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)

[<TodoListDetailsProperty>]
let ``TodoListDetails.applyEvent ItemArchived`` (aggregate: TodoListDetails) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemArchived(itemId)

    let expected =
        aggregate
        |> removeItemOnExisting itemId

    let actual =
        TodoListDetails.projection.ApplyEvent aggregate event

    Assert.Equal(expected, actual)
