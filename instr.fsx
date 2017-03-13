module Instruction
#load "types.fsx"
open Types
open Microsoft.FSharp.Core.Operators.Checked


let fetch machineState =
    let PC = machineState.Registers.[R15]
    machineState.Memory.[int (PC)/4]

let secondOp flexOp machineState =
  match flexOp with
  | Const n -> int n
  | Shift (b, Rn) -> machineState.Registers.[Rn] <<< int32 b

let boolToInt = function
    | true -> 1
    | false -> 0

let writeRegister rd machineState res =
  Map.add rd res machineState.Registers

let flagWrap flags f x y =
  let res =
    try
      (f x y), flags
    with
      | :? System.OverflowException as ex  ->
          printf "overflow!";
          let uncheckedRes = (Microsoft.FSharp.Core.Operators.(+) x y), {flags with V=true}
          uncheckedRes
  match res with
  | n, f when n=0 -> n, {f with Z = true}
  | n, f when n<0 -> n, {f with N = true}
  | _ -> res

let getAddFlags carry x y =
    let a, b = uint64 (uint32 x + (carry |> uint32)), uint64 (uint32 y)
    let c, d = int64 x, int64 y
    let ures = a + b + uint64 carry
    let sres = c + d + int64 carry
    let carry = match ures >>> 32 with
                   | 1UL -> true
                   | _ -> false
    let zero, neg = match (int sres) with
                    | 0 -> true, false
                    | n when n < 0 -> false, true
                    | _ -> false, false
    let overflow = match sres with
                   | n when n > 2147483647L -> true
                   | n when n < -2147483648L -> true
                   | _ -> false
    int sres, {N=neg; Z=zero; C=carry; V=overflow}

let getFlags f x y =
   let res = f x y
   let myFlags = {N=false; Z=false; C=false; V=false}
   let resFlags = match res with
                  | 0 ->  {myFlags with Z=true}
                  | n when n < 0 -> {myFlags with N=true}
                  | _ ->  myFlags
   res, myFlags



let execArithLogicInstr (arithLogicInstr:ArithLogicInstr) machineState =
    let opMatch aluOp =
        match aluOp with
        | AND -> (&&&), getFlags (&&&)
        | EOR -> (^^^), getFlags (^^^)
        | SUB -> (-), fun x y -> (getAddFlags 0) x (-y)
        | RSB -> (fun a b -> b - a), fun x y -> (getAddFlags 0) y (-x)
        | ADD -> (+), getAddFlags 0
        | ADC -> (fun a b -> (a + b + (machineState.CPSR.C |> boolToInt))), (getAddFlags (machineState.CPSR.C |> boolToInt))
        | SBC -> (fun a b -> (a - b - 1 + (machineState.CPSR.C |> boolToInt))), fun x y -> (getAddFlags (machineState.CPSR.C |> boolToInt)) x (-y-1)
        | RSC -> (fun a b -> (b - a - 1 + (machineState.CPSR.C |> boolToInt))), fun x y -> (getAddFlags (machineState.CPSR.C |> boolToInt)) y (-x-1)
        | ORR -> (|||), getFlags (|||)
        | BIC -> (fun x y -> ~~~x &&& y), getFlags (fun x y -> ~~~x &&& y)

    let arithLogicFun = (opMatch (arithLogicInstr.Op) |> fst)
    let flagFun = (opMatch (arithLogicInstr.Op) |> snd)
    let Op1, Op2 = (machineState.Registers.[arithLogicInstr.Rn]), (secondOp arithLogicInstr.Op2 machineState)

    let res, flags =
      match arithLogicInstr.S with
      | false -> arithLogicFun Op1 Op2, machineState.CPSR
      | true -> flagFun Op1 Op2

    {machineState with Registers = writeRegister arithLogicInstr.Rd machineState res; CPSR=flags}


let execMoveInstr (movInstr:MoveInstr) machineState =
    let opMatch movOp =
      match movOp with
      | MOV -> id
      | MVN -> (~~~)
    let secondOp = secondOp movInstr.Op2 machineState
    let res = opMatch movInstr.Op (secondOp)
    {machineState with Registers=writeRegister movInstr.Rd machineState res}

let execShiftInstr (shiftInstr:ShiftInstr) machineState =
    let rotateRight reg shift =
      let longN = int64 n
      let rotated = longN <<< 32 - shift%32
      let res = (longN ||| rotated)
    let opMatch = function
      | ASR -> (>>>)
      | LSR -> fun a b -> int ((uint32 a) >>> b)
      | ROR -> (>>>)
      | LSL -> (<<<)
      | RRX -> (>>>)
    let secondOp = secondOp shiftInstr.Op2 machineState
    let firstOp = machineState.Registers.[shiftInstr.Rn]
    let res = opMatch (shiftInstr.Op) (firstOp) (secondOp)
    {machineState with Registers=writeRegister shiftInstr.Rd machineState res}

let decode (possiblyInstr:PossiblyDecodedWord) =
    match possiblyInstr with
    | Word _ -> failwithf "Word decoded"
    | Instr instr ->
        match instr with
        | ArithLogicInstr instr -> fun () -> execArithLogicInstr instr
        | MoveInstr instr -> fun () -> execMoveInstr instr
        | TestInstr _ -> failwithf "not implemented"
        | MultInstr _ -> failwithf "not implemented"
        | ShiftInstr instr -> fun () -> execShiftInstr instr
        | BranchInstr _ -> failwithf "not implemented"
        | PSRInstr _ -> failwithf "not implemented"
        | MemInstr _ -> failwithf "not implemented"
        | MiscInstr _ -> failwithf "not implemented"
        | _ -> failwithf "not implemented"

let execute arg = fun x -> x() arg

let pipeLine machineState =
  machineState
  |> fetch
  |> decode
  |> execute {machineState with Registers= writeRegister R15 machineState (machineState.Registers.[R15] + 4)}

let rec execWrapper machineState:MachineRepresentation =
  let stoppingCondition = (machineState.Registers.[R15]/4 = List.length (machineState.Memory))
  match stoppingCondition with
  | true -> machineState
  | false -> execWrapper (pipeLine machineState)
