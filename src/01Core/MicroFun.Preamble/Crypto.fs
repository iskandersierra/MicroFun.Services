[<RequireQualifiedAccess>]
module MicroFun.Crypto

open System
open System.Security.Cryptography

[<RequireQualifiedAccess>]
module Random =
    let fillSpan (span: byte Span) =
        RandomNumberGenerator.Fill(span)

    let fillBytes (bytes: byte[]) =
        let span = bytes.AsSpan()
        RandomNumberGenerator.Fill(span)

    let getBytes (count: int) =
        RandomNumberGenerator.GetBytes(count)

[<RequireQualifiedAccess>]
module Sha1 =
    let hash (bytes: byte[]) =
        use sha1 = SHA1.Create()
        sha1.ComputeHash(bytes)
