module Sample =
  open ProceduralComputationExpression.Opt
  //open ProceduralComputationExpression.Cont

  let f1 () = proc {
    return 1
  }
  let f2 () = proc {
    return 10
    return 20
  }
  let f3 () = proc {
    ()
  }
  let f4 () = proc {
    ()
    return 100
  }
  let f5 cond () = proc {
    if cond then
      return "t"
    return "f"
  }
  let f6 max () = proc {
    let mutable i = 0
    while i < max do
      if i = 5 then
        return true
      i <- i + 1
    return false
  }

open Sample

type TestResult<'T> =
  | Green of 'T
  | Red of string

let test name f expected =
  try
    let res = f ()
    match expected with
    | Green x ->
        if res <> x then
          printfn "test %s...Failure. Expect: %A, Actual: %A" name x res
    | Red x ->
        printfn "test %s...Failure. Expect: err, Actual: %A" name res
  with
    e ->
      match expected with
      | Green x ->
          printfn "test %s...Failure. Expect: %A, Actual: err(%s)" name x e.Message
      | Red x ->
          if x <> e.Message then
            printfn "test %s...Failure. Expect: err(%s), Actual: err(%s)" name x e.Message

[<EntryPoint>]
let main argv =
  test "f1" Sample.f1 (Green 1)
  test "f2" Sample.f2 (Green 10)
  test "f3" Sample.f3 (Red "oops!")
  test "f4" Sample.f4 (Green 100)
  test "f5(t)" (Sample.f5 true) (Green "t")
  test "f5(f)" (Sample.f5 false) (Green "f")
  test "f6(t)" (Sample.f6 10) (Green true)
  test "f6(f)" (Sample.f6 3) (Green false)
  0
