[<RequireQualifiedAccess>]
module MicroFun.Seq

open System.Collections.Generic

let inline getEnumerator (source: 'a seq) = source.GetEnumerator()

let inline moveNext (enumerator: IEnumerator<'a>) = enumerator.MoveNext()
let inline current (enumerator: IEnumerator<'a>) = enumerator.Current

let inline tryMoveNext enumerator =
    if moveNext enumerator then
        Some (current enumerator)
    else
        None
