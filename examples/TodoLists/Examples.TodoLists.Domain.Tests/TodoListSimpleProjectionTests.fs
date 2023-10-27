module Examples.TodoLists.Domain.Tests.TodoListSimpleTests

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

type TodoListSimpleArb() =
    static member TodoListSimpleState() : Arbitrary<TodoListSimpleState> =
        let mapper title = { TodoListSimpleState.title = title }

        let unmapper (entity: TodoListSimpleState) = entity.title

        TodoListValueTypesArb.TodoListTitle()
        |> Arb.convert mapper unmapper

    static member TodoListSimple() : Arbitrary<TodoListSimple> =
        let generator =
            let none = Gen.constant TodoListSimple.None
            let archived = Gen.constant TodoListSimple.Archived

            let existing =
                TodoListSimpleArb.TodoListSimpleState().Generator
                |> Gen.map TodoListSimple.Existing

            Gen.frequency [ 1, none
                            1, archived
                            8, existing ]

        let shrinker =
            function
            | TodoListSimple.None -> Seq.empty
            | TodoListSimple.Archived -> Seq.empty
            | TodoListSimple.Existing state ->
                TodoListSimpleArb
                    .TodoListSimpleState()
                    .Shrinker state
                |> Seq.map TodoListSimple.Existing

        Arb.fromGenShrink (generator, shrinker)

[<AttributeUsage(AttributeTargets.Method
                 ||| AttributeTargets.Property,
                 AllowMultiple = false)>]
type TodoListSimplePropertyAttribute() as this =
    inherit PropertyAttribute()

    do
        this.Verbose <- Verbose

        this.Arbitrary <-
            [| typeof<TodoListValueTypesArb>
               typeof<TodoListSimpleArb> |]


[<Fact>]
let ``TodoListSimple.initial should be None`` () =
    let expected = TodoListSimple.None
    let actual = TodoListSimple.projection.InitialItem

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent Created`` (state: TodoListSimple) (title: TodoListTitle) =
    let event = TodoListEvent.Created(title)

    let expected =
        TodoListSimple.Existing { TodoListSimpleState.title = title }

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent TitleChanged`` (state: TodoListSimple) (title: TodoListTitle) =
    let event = TodoListEvent.TitleChanged(title)

    let expected =
        state
        |> TodoListSimple.mapExisting (TodoListSimpleState.setTitle title)

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent Archived`` (state: TodoListSimple) =
    let event = TodoListEvent.Archived

    let expected =
        state
        |> TodoListSimple.bindExisting (konst TodoListSimple.Archived)

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent ItemAdded`` (state: TodoListSimple) (itemId: TodoItemId) (title: TodoItemTitle) =
    let event = TodoListEvent.ItemAdded(itemId, title)

    let expected = state

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent ItemTitleChanged`` (state: TodoListSimple) (itemId: TodoItemId) (title: TodoItemTitle) =
    let event =
        TodoListEvent.ItemTitleChanged(itemId, title)

    let expected = state

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent ItemCompleted`` (state: TodoListSimple) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemCompleted(itemId)

    let expected = state

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent ItemReopened`` (state: TodoListSimple) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemReopened(itemId)

    let expected = state

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)

[<TodoListSimpleProperty>]
let ``TodoListSimple.applyEvent ItemArchived`` (state: TodoListSimple) (itemId: TodoItemId) =
    let event = TodoListEvent.ItemArchived(itemId)

    let expected = state

    let actual =
        TodoListSimple.projection.ApplyEvent state event

    Assert.Equal(expected, actual)
