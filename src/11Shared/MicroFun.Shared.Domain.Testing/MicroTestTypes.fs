namespace MicroFun.Shared.Domain.Testing

[<Struct>]
type Word =
    | Word of value: string
    static member getValue (Word value) = value
    member this.Get =
        match this with
        | Word value -> value

[<Struct>]
type FirstWord =
    | FirstWord of value: string
    static member getValue (FirstWord value) = value
    member this.Get =
        match this with
        | FirstWord value -> value

[<Struct>]
type Sentence =
    | Sentence of value: string
    static member getValue (Sentence value) = value
    member this.Get =
        match this with
        | Sentence value -> value

[<Struct>]
type Paragraph =
    | Paragraph of value: string
    static member getValue (Paragraph value) = value
    member this.Get =
        match this with
        | Paragraph value -> value
