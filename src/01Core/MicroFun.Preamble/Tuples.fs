namespace MicroFun

[<RequireQualifiedAccess>]
module Tuple2 =
    let inline map fn1 fn2 (a, b) = (fn1 a, fn2 b)

[<RequireQualifiedAccess>]
module Tuple3 =
    let inline map fn1 fn2 fn3 (a, b, c) = (fn1 a, fn2 b, fn3 c)

[<RequireQualifiedAccess>]
module Tuple4 =
    let inline map fn1 fn2 fn3 fn4 (a, b, c, d) = (fn1 a, fn2 b, fn3 c, fn4 d)

[<RequireQualifiedAccess>]
module Tuple5 =
    let inline map fn1 fn2 fn3 fn4 fn5 (a, b, c, d, e) = (fn1 a, fn2 b, fn3 c, fn4 d, fn5 e)
