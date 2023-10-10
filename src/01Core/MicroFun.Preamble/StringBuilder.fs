namespace MicroFun

open System
open System.Text


[<RequireQualifiedAccess>]
module StringBuilderCE =
    type Delayed = StringBuilder -> unit


    let run (sb: StringBuilder) (delayed: Delayed) : string =
        do delayed sb
        sb.ToString()

    let inline delay (f: unit -> Delayed) : Delayed = fun sb -> (f ()) sb

    let zero: Delayed = ignore

    let combine (f: Delayed) (g: Delayed) : Delayed =
        fun sb ->
            do f sb
            do g sb

    let inline yieldString (v: string) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldCharArray (v: char []) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldChar (v: char) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldStringBuilder (v: StringBuilder) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldObj (v: obj) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldBool (v: bool) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt8 (v: uint8) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt16 (v: uint16) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt32 (v: uint32) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldUInt64 (v: uint64) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt8 (v: int8) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt16 (v: int16) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt32 (v: int32) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldInt64 (v: int64) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldDecimal (v: decimal) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldFloat (v: float) : Delayed = fun sb -> sb.Append(v) |> ignore

    let inline yieldFloat32 (v: float32) : Delayed = fun sb -> sb.Append(v) |> ignore


    let tryWith (handler: exn -> Delayed) (body: Delayed) : Delayed =
        fun sb ->
            try
                body sb
            with
            | exn -> (handler exn) sb

    let tryFinally (handler: unit -> unit) (body: Delayed) : Delayed =
        fun sb ->
            try
                body sb
            finally
                handler ()

    let using (body: 'd -> Delayed) (disposable: #IDisposable) =
        tryFinally (fun () -> dispose disposable) (fun sb -> body disposable sb)

    let rec whileLoop (body: Delayed) (guard: unit -> bool) : Delayed =
        fun sb ->
            if guard () then
                body sb
                whileLoop body guard sb
            else
                zero sb

    let forLoop (body: 'a -> Delayed) (source: 'a seq) =
        Seq.getEnumerator source
        |> using (fun enumerator ->
            (fun () -> Seq.moveNext enumerator)
            |> whileLoop (fun sb -> (enumerator |> Seq.current |> body) sb))


    type StringBuilderCEBuilder(?initialCapacity: int, ?initialValue: string) =
        let createSB () =
            match initialCapacity, initialValue with
            | None, None -> StringBuilder()
            | None, Some value -> StringBuilder(value)
            | Some capacity, None -> StringBuilder(capacity)
            | Some capacity, Some value -> StringBuilder(value, capacity)

        member this.Run delayed = run (createSB ()) delayed

        member this.Delay delayed = delay delayed

        member this.Zero() = ignore

        member this.Combine(f, g) = combine f g


        member this.Yield value = yieldString value

        member this.Yield value = yieldCharArray value

        member this.Yield value = yieldChar value

        member this.Yield value = yieldStringBuilder value

        member this.Yield value = yieldBool value

        member this.Yield value = yieldObj value

        member this.Yield value = yieldUInt8 value

        member this.Yield value = yieldUInt16 value

        member this.Yield value = yieldUInt32 value

        member this.Yield value = yieldUInt64 value

        member this.Yield value = yieldInt8 value

        member this.Yield value = yieldInt16 value

        member this.Yield value = yieldInt32 value

        member this.Yield value = yieldInt64 value

        member this.Yield value = yieldDecimal value

        member this.Yield value = yieldFloat value

        member this.Yield value = yieldFloat32 value


        member this.TryWith(body, handler) = tryWith handler body 

        member this.TryFinally(body, handler) = tryFinally handler body 

        member this.Using(disposable, body) = using body disposable 


        member this.While(guard, body) = whileLoop body guard

        member this.For(source, body) = forLoop body source


[<AutoOpen>]
module StringBuilderCEBuiltIns =
    let stringBuilder = StringBuilderCE.StringBuilderCEBuilder()

    let stringBuilderWith capacity =
        StringBuilderCE.StringBuilderCEBuilder(initialCapacity = capacity)

    let stringBuilderFrom value =
        StringBuilderCE.StringBuilderCEBuilder(initialValue = value)
