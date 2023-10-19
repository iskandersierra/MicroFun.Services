namespace MicroFun.Shared.Domain

type IItemProjection<'item, 'event> =
    abstract member InitialItem : 'item

    abstract member ApplyEvent : 'item -> 'event -> 'item
