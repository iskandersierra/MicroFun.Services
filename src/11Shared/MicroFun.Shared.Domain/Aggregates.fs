namespace MicroFun.Shared.Domain

open Validus

type IStateAggregate<'state, 'command, 'event> =
    abstract member ExecuteCommand: 'state -> 'command -> ValidationResult<'event list>

[<RequireQualifiedAccess>]
module Aggregate =
    [<Literal>]
    let State = "State"

    let createErrors errors = Error(ValidationErrors.create State errors)

    let createError error = createErrors [ error ]
