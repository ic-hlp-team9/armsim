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

let execArithLogicInstr (arithLogicInstr:ArithLogicInstr) machineState =
    let opMatch aluOp =
        match aluOp with
        | AND -> (&&&)
        | EOR -> (^^^)
        | SUB -> (-)
        | RSB -> fun a b -> b - a
        | ADD -> (+)
        | ADC -> fun a b -> (a + b + System.Convert.ToInt32(machineState.CPSR.C))
        | SBC -> fun a b -> (a - b - 1 + System.Convert.ToInt32(machineState.CPSR.C))
        | RSC -> fun a b -> (b - a - 1 + System.Convert.ToInt32(machineState.CPSR.C))
        | ORR -> (|||)
        | BIC -> (+)

    let res, flags =
      match arithLogicInstr.S with
      | false -> (opMatch (arithLogicInstr.Op) (machineState.Registers.[arithLogicInstr.Rn]) (secondOp arithLogicInstr.Op2 machineState)), machineState.CPSR
      | true -> flagWrap machineState.CPSR (opMatch (arithLogicInstr.Op)) (machineState.Registers.[arithLogicInstr.Rn]) (secondOp arithLogicInstr.Op2 machineState)

    let res = opMatch (arithLogicInstr.Op) (machineState.Registers.[arithLogicInstr.Rn]) (secondOp arithLogicInstr.Op2 machineState)
    {machineState with Registers = writeRegister arithLogicInstr.Rd machineState res}


let execMoveInstr (movInstr:MoveInstr) machineState =
    let opMatch movOp =
      match movOp with
      | MOV -> id
      | MVN -> (~~~)
    let secondOp = secondOp movInstr.Op2 machineState
    let res = opMatch movInstr.Op (secondOp)
    {machineState with Registers=writeRegister movInstr.Rd machineState res}


let decode (possiblyInstr:PossiblyDecodedWord) =
    match possiblyInstr with
    | Word _ -> failwithf "Word decoded"
    | Instr instr ->
        match instr with
        | ArithLogicInstr instr -> fun () -> execArithLogicInstr instr
        | MoveInstr instr -> fun () -> execMoveInstr instr
        | TestInstr _ -> failwithf "not implemented"
        | MultInstr _ -> failwithf "not implemented"
        | ShiftInstr _ -> failwithf "not implemented"
        | BranchInstr _ -> failwithf "not implemented"
        | PSRInstr _ -> failwithf "not implemented"
        | MemInstr _ -> failwithf "not implemented"
        | MiscInstr _ -> failwithf "not implemented"
        | _ -> failwithf "not implemented"


let execute m = fun x -> x() m

let pipeLine machineState =
  machineState
  |> fetch
  |> decode
  |> execute {machineState with Registers= writeRegister R15 machineState (machineState.Registers.[R15] + 4)}


let rec execWrapper machineState:MachineRepresentation =
  match true with
  | true -> machineState
  | false -> pipeLine machineState
