namespace Examples.TodoLists.Domain.Tests

open Examples.TodoLists.Domain
open FsCheck
open MicroFun.Shared.Domain.Testing


type TodoListValueTypesArb() =
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

