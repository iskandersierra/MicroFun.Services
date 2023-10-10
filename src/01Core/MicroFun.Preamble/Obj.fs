[<RequireQualifiedAccess>]
module MicroFun.Obj

let inline eq a b = obj.Equals(a, b)

let inline refEq a b = obj.ReferenceEquals(a, b)

let inline isNull a = refEq a null

let inline isNotNull a = not (isNull a)
