[<AutoOpen>]
module Client.Common.Json


let inline fromJson<'T> (str: string): Result<'T, exn> =
#if FABLE_COMPILER
  let extraCoders =
    Thoth.Json.Extra.empty
    |> Thoth.Json.Extra.withInt64
  try Thoth.Json.Decode.Auto.fromString<'T>(str, false, extraCoders)
      |> Result.mapError (fun x -> exn x)
  with ex -> Error ex
#else
  let extraCoders =
    Thoth.Json.Net.Extra.empty
    |> Thoth.Json.Net.Extra.withInt64
  try Thoth.Json.Net.Decode.Auto.fromString<'T>(str, false, extraCoders)
      |> Result.mapError (fun x -> exn x)
  with ex -> Error ex
#endif


let toJson obj =
#if FABLE_COMPILER
  Thoth.Json.Encode.Auto.toString(0, obj)
#else
  Thoth.Json.Net.Encode.Auto.toString(0, obj)
#endif
