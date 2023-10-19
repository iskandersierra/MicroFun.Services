[<RequireQualifiedAccess>]
module MicroFun.Seq

open System.Collections.Generic

let inline getEnumerator (source: 'a seq) = source.GetEnumerator()

let inline moveNext (enumerator: IEnumerator<'a>) = enumerator.MoveNext()
let inline current (enumerator: IEnumerator<'a>) = enumerator.Current

let inline tryMoveNext enumerator =
    if moveNext enumerator then
        Some(current enumerator)
    else
        None


let scanWhile
    (folder: 'state
                 -> 'a
                 -> struct {| stop: bool
                              state: 'state voption |})
    (state: 'state)
    (source: 'a seq)
    =
    seq {
        use enumerator = getEnumerator source

        let rec loop state =
            seq {
                match tryMoveNext enumerator with
                | Some value ->
                    let result = folder state value

                    match result.state, result.stop with
                    | ValueSome state', false ->
                        yield state'
                        yield! loop state'
                    | ValueSome state', true -> yield state'
                    | ValueNone, false -> yield! loop state
                    | ValueNone, true -> ()

                | None -> ()
            }

        yield state
        yield! loop state
    }

let inline foldWhile folder state = scanWhile folder state >> Seq.last

let takeAtLeast
    (count: int)
    (source: 'a seq) =
    seq {
        use enumerator = getEnumerator source

        let rec loop index =
            seq {
                if index < count then
                    match tryMoveNext enumerator with
                    | Some value ->
                        yield value
                        yield! loop (index + 1)
                    | None -> ()
                else
                    ()
            }

        yield! loop 0
    }
