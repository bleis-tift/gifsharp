namespace ProceduralComputationExpression.Cont

type ContType<'T> =
  | Continue
  | Value of 'T

type ProceduralBuilder () =
  member this.Zero() = fun k -> k Continue
  member this.Return(x) = fun _ -> Value x
  member this.Combine(kx, rest) =
    fun k -> kx (fun _ -> rest () k)
  member this.While(guard, f) =
    if not (guard ()) then this.Zero()
    else
      let x = f ()
      this.Combine(x, fun () -> this.While(guard, f))
  member this.Delay(f: unit -> (_ -> ContType<_>)) = f
  member this.Run(f: unit -> (_ -> ContType<_>)) =
    match f () id with
    | Value x -> x
    | Continue -> failwith "oops!"

[<AutoOpen>]
module Procedural =
  let proc = ProceduralBuilder()
