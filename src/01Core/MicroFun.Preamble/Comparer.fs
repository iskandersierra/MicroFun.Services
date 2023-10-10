namespace MicroFun

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Comparer =
    let create f =
        { new IComparer<_> with
            member __.Compare(x, y) = f x y }

    let inline isLessThan (comparer: IComparer<_>) refValue value =
        comparer.Compare(value, refValue) < 0

    let inline isLessThanOrEqualTo (comparer: IComparer<_>) refValue value =
        comparer.Compare(value, refValue) <= 0

    let inline isGreaterThan (comparer: IComparer<_>) refValue value =
        comparer.Compare(value, refValue) > 0

    let inline isGreaterThanOrEqualTo (comparer: IComparer<_>) refValue value =
        comparer.Compare(value, refValue) >= 0

    let clamp (comparer: IComparer<_>) minValue maxValue value =
        if comparer.Compare(value, minValue) < 0 then
            minValue
        elif comparer.Compare(value, maxValue) > 0 then
            maxValue
        else
            value

    let inline isBetween (comparer: IComparer<_>) minValue maxValue value =
        (value |> isGreaterThanOrEqualTo comparer minValue) &&
        (value |> isLessThanOrEqualTo comparer maxValue)

    let inline isBetweenExclusive (comparer: IComparer<_>) minValue maxValue value =
        (value |> isGreaterThan comparer minValue) &&
        (value |> isLessThan comparer maxValue)

    let inline isNotBetween comparer minValue maxValue =
        isBetween comparer minValue maxValue >> not

    let inline isNotBetweenExclusive comparer minValue maxValue =
        isBetweenExclusive comparer minValue maxValue >> not


[<AutoOpen>]
module ComparerPreamble =
    type IComparer<'a> with
        member this.IsLessThan(value, refValue) =
            value |> Comparer.isLessThan this refValue

        member this.IsLessThanOrEqualTo(value, refValue) =
            value |> Comparer.isLessThanOrEqualTo this refValue

        member this.IsGreaterThan(value, refValue) =
            value |> Comparer.isGreaterThan this refValue

        member this.IsGreaterThanOrEqualTo(value, refValue) =
            value |> Comparer.isGreaterThanOrEqualTo this refValue


        member this.Clamp(value, minValue, maxValue) = value |> Comparer.clamp this minValue maxValue


        member this.IsBetween(value, minValue, maxValue) =
            value |> Comparer.isBetween this minValue maxValue

        member this.IsBetweenExclusive(value, minValue, maxValue) =
            value |> Comparer.isBetweenExclusive this minValue maxValue

        member this.IsNotBetween(value, minValue, maxValue) =
            value |> Comparer.isNotBetween this minValue maxValue

        member this.IsNotBetweenExclusive(value, minValue, maxValue) =
            value |> Comparer.isNotBetweenExclusive this minValue maxValue


    let inline isLessThan refValue value =
        value < refValue

    let inline isLessThanOrEqualTo refValue value =
        value <= refValue

    let inline isGreaterThan refValue value =
        value > refValue

    let inline isGreaterThanOrEqualTo refValue value =
        value >= refValue

    let clamp minValue maxValue value =
        if value < minValue then
            minValue
        elif value > maxValue then
            maxValue
        else
            value

    let inline isBetween minValue maxValue value =
        value >= minValue && value <= maxValue

    let inline isBetweenExclusive minValue maxValue value =
        value > minValue && value < maxValue

    let inline isNotBetween minValue maxValue =
        isBetween minValue maxValue >> not

    let inline isNotBetweenExclusive minValue maxValue =
        isBetweenExclusive minValue maxValue >> not
