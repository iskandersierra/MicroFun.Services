namespace MicroFun

open System
open System.Globalization
open System.Text

[<RequireQualifiedAccess>]
module String =
    let inline toEncodedBytes (encoding: Encoding) (source: string) = encoding.GetBytes(source)
    let inline ofEncodedBytes (encoding: Encoding) (bytes: byte []) = encoding.GetString(bytes)

    let toUtf8Bytes = toEncodedBytes Encoding.UTF8
    let ofUtf8Bytes = ofEncodedBytes Encoding.UTF8

    let toAsciiBytes = toEncodedBytes Encoding.ASCII
    let ofAsciiBytes = ofEncodedBytes Encoding.ASCII


    let inline isNullOrEmpty (source: string) = String.IsNullOrEmpty source
    let isNotNullOrEmpty = isNullOrEmpty >> not
    let inline isNullOrWhiteSpace (source: string) = String.IsNullOrWhiteSpace source
    let isNotNullOrWhiteSpace = isNullOrWhiteSpace >> not

    let inline emptyWhenNull (source: string) = if isNull source then String.Empty else source


    let inline indexOf (value: string) (source: string) = source.IndexOf(value)
    let inline indexOfChar (value: char) (source: string) = source.IndexOf(value)
    let inline indexOfAny (values: char []) (source: string) = source.IndexOfAny(values)

    let inline indexOfAnyFrom (startIndex: int) (values: char []) (source: string) =
        source.IndexOfAny(values, startIndex)

    let inline indexOfWithComparison (comparison: StringComparison) (value: string) (source: string) =
        source.IndexOf(value, comparison)

    let inline indexOfFrom (startIndex: int) (value: string) (source: string) = source.IndexOf(value, startIndex)

    let inline indexOfWithComparisonFrom
        (startIndex: int)
        (comparison: StringComparison)
        (value: string)
        (source: string)
        =
        source.IndexOf(value, startIndex, comparison)


    let inline lastIndexOf (value: string) (source: string) = source.LastIndexOf(value)
    let inline lastIndexOfChar (value: char) (source: string) = source.LastIndexOf(value)
    let inline lastIndexOfAny (values: char []) (source: string) = source.LastIndexOfAny(values)

    let inline lastIndexOfAnyFrom (startIndex: int) (values: char []) (source: string) =
        source.LastIndexOfAny(values, startIndex)

    let inline lastIndexOfWithComparison (comparison: StringComparison) (value: string) (source: string) =
        source.LastIndexOf(value, comparison)

    let inline lastIndexOfFrom (startIndex: int) (value: string) (source: string) =
        source.LastIndexOf(value, startIndex)

    let inline lastIndexOfWithComparisonFrom
        (startIndex: int)
        (comparison: StringComparison)
        (value: string)
        (source: string)
        =
        source.LastIndexOf(value, startIndex, comparison)


    let inline insertAt (startIndex: int) (value: string) (source: string) = source.Insert(startIndex, value)
    let prepend = insertAt 0


    let inline padLeft (totalWidth: int) (source: string) = source.PadLeft(totalWidth)
    let inline padLeftWith (char: char) (totalWidth: int) (source: string) = source.PadLeft(totalWidth, char)


    let inline padRight (totalWidth: int) (source: string) = source.PadRight(totalWidth)
    let inline padRightWith (char: char) (totalWidth: int) (source: string) = source.PadRight(totalWidth, char)


    let inline replace (oldValue: string) (newValue: string) (source: string) = source.Replace(oldValue, newValue)
    let inline replaceChar (oldValue: char) (newValue: char) (source: string) = source.Replace(oldValue, newValue)

    let inline replaceWithComparison
        (comparison: StringComparison)
        (oldValue: string)
        (newValue: string)
        (source: string)
        =
        source.Replace(oldValue, newValue, comparison)

    let inline replaceWithCulture
        (ignoreCase: bool)
        (culture: CultureInfo)
        (oldValue: string)
        (newValue: string)
        (source: string)
        =
        source.Replace(oldValue, newValue, ignoreCase, culture)


    let inline endsWith (prefix: string) (source: string) = source.EndsWith(prefix)
    let inline endsWithChar (prefix: char) (source: string) = source.EndsWith(prefix)

    let inline endsWithComparison (comparison: StringComparison) (prefix: string) (source: string) =
        source.EndsWith(prefix, comparison)

    let inline endsWithCulture (ignoreCase: bool) (culture: CultureInfo) (prefix: string) (source: string) =
        source.EndsWith(prefix, ignoreCase, culture)


    let inline startsWith (prefix: string) (source: string) = source.StartsWith(prefix)
    let inline startsWithChar (prefix: char) (source: string) = source.StartsWith(prefix)

    let inline startsWithComparison (comparison: StringComparison) (prefix: string) (source: string) =
        source.StartsWith(prefix, comparison)

    let inline startsWithCulture (ignoreCase: bool) (culture: CultureInfo) (prefix: string) (source: string) =
        source.StartsWith(prefix, ignoreCase, culture)


    let inline contains (value: string) (source: string) = source.Contains(value)
    let inline containsChar (value: char) (source: string) = source.Contains(value)

    let inline containsComparison (comparison: StringComparison) (value: string) (source: string) =
        source.Contains(value, comparison)


    let inline substring startIndex length (source: string) = source.Substring(startIndex, length)
    let inline substringFrom startIndex (source: string) = source.Substring(startIndex)


    let inline remove startIndex length (source: string) = source.Remove(startIndex, length)
    let inline removeFrom startIndex (source: string) = source.Remove(startIndex)


    let inline replaceLineEndings (source: string) = source.ReplaceLineEndings()
    let inline replaceLineEndingsWith (value: string) (source: string) = source.ReplaceLineEndings(value)


    let inline trim (source: string) = source.Trim()
    let inline trimChars (chars: char []) (source: string) = source.Trim(chars)
    let inline trimChar (char: char) (source: string) = source.Trim(char)

    let inline trimStart (source: string) = source.TrimStart()
    let inline trimStartChars (chars: char []) (source: string) = source.TrimStart(chars)
    let inline trimStartChar (char: char) (source: string) = source.TrimStart(char)

    let inline trimEnd (source: string) = source.TrimEnd()
    let inline trimEndChars (chars: char []) (source: string) = source.TrimEnd(chars)
    let inline trimEndChar (char: char) (source: string) = source.TrimEnd(char)


    let inline splitBy (separator: string) (source: string) = source.Split(separator)

    let inline splitByAny (separators: string []) (source: string) =
        source.Split(separators, StringSplitOptions.None)

    let inline splitByChar (separator: char) (source: string) = source.Split(separator)
    let inline splitByAnyChar (separators: char []) (source: string) = source.Split(separators)


    let inline toChars (source: string) = source.ToCharArray()
    let inline ofChars (chars: char []) = String(chars)
    let inline ofCharsFrom (startIndex: int) (length: int) (chars: char []) = String(chars, startIndex, length)
    let inline ofNChars (count: int) (char: char) = String(char, count)


    let inline toLower (source: string) = source.ToLower()
    let inline toLowerWithCulture (culture: CultureInfo) (source: string) = source.ToLower(culture)
    let inline toLowerInvariant (source: string) = source.ToLowerInvariant()


    let inline toUpper (source: string) = source.ToUpper()
    let inline toUpperWithCulture (culture: CultureInfo) (source: string) = source.ToUpper(culture)
    let inline toUpperInvariant (source: string) = source.ToUpperInvariant()
