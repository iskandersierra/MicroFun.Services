namespace MicroFun.Shared.Domain

open MicroFun
open System.Text.RegularExpressions

[<Struct>]
type EncodedError =
    { code: string
      data: Map<string, string> }

[<RequireQualifiedAccess>]
module EncodedError =
    [<Literal>]
    let DataSeparator = "|"

    let KVSeparator = "="

    let create code data =
        { code = code
          data = data }

    let format (error: EncodedError) =
        stringBuilder {
            yield error.code

            for (key, value) in error.data |> Map.toSeq do
                yield DataSeparator
                yield key
                yield "="
                yield value
        }

    let formattedRegex =
        let sep = Regex.Escape DataSeparator
        let kvsep = Regex.Escape KVSeparator
        let code = $@"(?<code>[^{sep}]+)"
        let kvp =
            $@"{sep}(?<key>[^{kvsep}{sep}]+){kvsep}(?<value>[^{sep}]+)"
        let field = $@" at (?<field>.+)"
        let formatted = $@"^{code}({kvp})*$"

        Regex(formatted, RegexOptions.Compiled)

    let tryParse (value: string) =
        let match' = formattedRegex.Match value

        if match'.Success then
            let code = match'.Groups.["code"].Value

            let data =
                let keys =
                    match'.Groups.["key"].Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun m -> m.Value)

                let values =
                    match'.Groups.["value"].Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun m -> m.Value)

                Seq.zip keys values |> Map.ofSeq

            Some { code = code; data = data }
        else
            None
