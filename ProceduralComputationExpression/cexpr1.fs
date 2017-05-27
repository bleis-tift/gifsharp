namespace ProceduralComputationExpression.Opt

type ProcResult<'T> =
  | NotReturn
  | Return of 'T

type ProceduralBuilder () =
  member this.Zero() = NotReturn
  member this.Return(x) = Return x
  member this.Combine(x, rest) =
    match x with
    | Return x -> Return x
    | NotReturn -> rest ()
  member this.While(guard, f) =
    if not (guard ()) then this.Zero()
    else
      let x = f ()
      this.Combine(x, fun () -> this.While(guard, f))
  member this.Delay(f: unit -> ProcResult<_>) = f
  member this.Run(f: unit -> ProcResult<_>) =
    match f () with
    | Return x -> x
    | NotReturn -> failwith "oops!"

[<AutoOpen>]
module Procedural =
  let proc = ProceduralBuilder()