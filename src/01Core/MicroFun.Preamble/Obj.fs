[<RequireQualifiedAccess>]
module MicroFun.Obj

let inline equals a b = obj.Equals(a, b)
let inline notEquals a b = not (equals a b)

let inline refEquals a b = obj.ReferenceEquals(a, b)
let inline notRefEquals a b = not (refEquals a b)

let inline isNull a = refEquals a null
let inline isNotNull a = notRefEquals a null
