namespace global


/// Helper functions to process Union Cases
[<RequireQualifiedAccess>]
module UnionProps =
  let values f props = props |> List.choose f

  let concat f props = props |> values f |> List.concat

  let tryLast f props = props |> List.rev |> List.tryPick f
