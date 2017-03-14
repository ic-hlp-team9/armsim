module Instruction
#load "types.fsx"
open Types
open Microsoft.FSharp.Core.Operators.Checked


let fetch machineState =
    let PC = machineState.Registers.[R15] |> uint32
    machineState.Memory.[PC]


let secondOp flexOp machineState =
  match flexOp with
  | Const n -> int n
  | Shift (b, Rn) -> machineState.Registers.[Rn] <<< int32 b


let boolToInt = function
    | true -> 1
    | false -> 0


let writeRegister rd machineState res =
  Map.add rd res machineState.Registers


let getMemWord (byteAddressing:bool) (address:Address) (machineState:MachineRepresentation) =
  let memContent= match byteAddressing with
                   | false when address%4u <> 0u -> failwithf "Unaligned memory access"
                   | false -> machineState.Memory.[address]
                   | true -> let byteOffset = address%4u; machineState.Memory.[address-byteOffset] |> (<<<) (3u-byteOffset) |> (>>>) 3u
  match memContent with

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


type ArithOperation = Addition | Subtraction
type ShiftOperation = Left | Right


let getAddFlags operation carry x y =
    let a = uint64 (uint32 x)
    let b = match operation with
            | Subtraction -> uint64 (~~~(uint32 y)) + 1UL
            | Addition -> uint64 (uint32 x)
    let c = int64 x
    let d = match operation with
            | Subtraction -> - (int64 y)
            | Addition -> int64 y
    let ures = a + b + (carry |> uint64)
    let sres = c + d + (carry |> int64)
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


let getShiftFlags shiftDir f x y =
  let carryCheck =
    match shiftDir with
    | Right -> 1
    | Left -> -2147483648
  let carryRes =
    match x with
    | 0 -> y
    | _ -> f x (y-1)
  let myFlags = {N=false; Z=false; C=(carryRes &&& carryCheck <> 0); V=false}
  let res = f x y
  let resFlags = match res with
                 | 0 ->  {myFlags with Z=true}
                 | n when n < 0 -> {myFlags with N=true}
                 | _ ->  myFlags
  res, myFlags


let execArithLogicInstr (arithLogicInstr:ArithLogicInstr) (machineState:MachineRepresentation) =
    let opMatch  = function
        | AND -> (&&&), getFlags (&&&)
        | EOR -> (^^^), getFlags (^^^)
        | SUB -> (-),  getAddFlags Subtraction 0
        | RSB -> (fun a b -> b - a), fun x y -> (getAddFlags Subtraction 0) y x
        | ADD -> (+), getAddFlags Addition 0
        | ADC -> (fun a b -> (a + b + (machineState.CPSR.C |> boolToInt))), (getAddFlags Addition (machineState.CPSR.C |> boolToInt))
        | SBC -> (fun a b -> (a - b - 1 + (machineState.CPSR.C |> boolToInt))), getAddFlags Subtraction (machineState.CPSR.C |> boolToInt |> (-) 1)
        | RSC -> (fun a b -> (b - a - 1 + (machineState.CPSR.C |> boolToInt))), fun x y -> (getAddFlags Subtraction (machineState.CPSR.C |> boolToInt |> (-) 1) y x)
        | ORR -> (|||), getFlags (|||)
        | BIC -> (fun x y -> ~~~x &&& y), getFlags (fun x y -> ~~~x &&& y)
    let arithLogicFun = (opMatch (arithLogicInstr.Op) |> fst)
    let flagFun = (opMatch (arithLogicInstr.Op) |> snd)
    let op1, op2 = (machineState.Registers.[arithLogicInstr.Rn]), (secondOp arithLogicInstr.Op2 machineState)
    let res, flags =
      match arithLogicInstr.S with
      | false -> arithLogicFun op1 op2, machineState.CPSR
      | true -> flagFun op1 op2
    {machineState with Registers = writeRegister arithLogicInstr.Rd machineState res; CPSR=flags}


let execTestInstr (testInstr:TestInstr) (machineState:MachineRepresentation) =
  let opMatch = function
    | TST -> getFlags (&&&)
    | TEQ -> getFlags (^^^)
    | CMP -> getAddFlags Subtraction 0
    | CMN -> getAddFlags Addition 0
  let op1, op2 = (machineState.Registers.[testInstr.Rn]), (secondOp testInstr.Op2 machineState)
  let flags = (testInstr.Op |> opMatch) op1 op2 |> snd
  {machineState with CPSR=flags}


let execMoveInstr (movInstr:MoveInstr) (machineState:MachineRepresentation) =
    let opMatch = function
      | MOV -> id, fun x -> (x, {N = (x < 0); Z = (x=0); C=false; V=false;})
      | MVN -> (~~~), fun x -> (~~~x, {N = (~~~x < 0); Z = (~~~x=0); C=false; V=false;})
    let movFun = (opMatch (movInstr.Op) |> fst)
    let flagFun = (opMatch (movInstr.Op) |> snd)
    let op = secondOp movInstr.Op2 machineState
    let res, flags =
      match movInstr.S with
      | false -> movFun op, machineState.CPSR
      | true -> flagFun op
    {machineState with Registers=writeRegister movInstr.Rd machineState res}


let execShiftInstr (shiftInstr:ShiftInstr) (machineState:MachineRepresentation) =
    let rotateRight reg shift =
        let longReg = int64 reg
        let rotated = longReg <<< (32 - shift%32)
        int (longReg >>> shift%32 ||| rotated)

    let rorateRightX _ a = machineState.CPSR.C |> boolToInt |> (<<<) 31 |> (|||) (a >>> 1)
    let logicalShiftLeft = fun a b -> int ((uint32 a) >>> b)

    let opMatch = function
      | ASR -> (>>>), getShiftFlags Right (>>>)
      | LSR -> logicalShiftLeft, getShiftFlags Right logicalShiftLeft
      | ROR -> rotateRight, getShiftFlags Right rotateRight
      | LSL -> (<<<), getShiftFlags Left (<<<)
      | RRX -> rorateRightX, getShiftFlags Right rorateRightX

    let shiftFun = (opMatch (shiftInstr.Op) |> fst)
    let flagFun = (opMatch (shiftInstr.Op) |> snd)
    let op1, op2 = machineState.Registers.[shiftInstr.Rn], secondOp shiftInstr.Op2 machineState

    let res, flags =
      match shiftInstr.S with
      | true -> shiftFun op1 op2, machineState.CPSR
      | false -> flagFun op1 op2
    {machineState with Registers=writeRegister shiftInstr.Rd machineState res}


let execBranchInstr (branchInstr:BranchInstr) (machineState:MachineRepresentation) =
  let dest = int branchInstr.Address
  let jumpState = {machineState with Registers = writeRegister R15 machineState dest}
  match branchInstr.L with
  | false -> jumpState
  | true ->
    let link = machineState.Registers.[R15] + 4;
    {jumpState with Registers = (writeRegister R14 machineState link)}


let execSingleMemString (memInstr:SingleMemInstr) (machineState:MachineRepresentation) =
  let offset = secondOp memInstr.Offset
  let loadPointer, resPointer =
    match memInstr.Addressing, machineState.Registers.[memInstr.Pointer] with
    | Pre, adr -> adr, adr+offset
    | Post, adr -> adr+offset, adr+offset
  match memInstr.


let decode (possiblyInstr:PossiblyDecodedWord) =
    match possiblyInstr with
    | Word _ -> failwithf "Word decoded"
    | Instr instr ->
        match instr with
        | ArithLogicInstr instr -> fun () -> execArithLogicInstr instr
        | MoveInstr instr -> fun () -> execMoveInstr instr
        | TestInstr instr -> fun () -> execTestInstr instr
        | MultInstr _ -> failwithf "not implemented"
        | ShiftInstr instr -> fun () -> execShiftInstr instr
        | BranchInstr instr -> fun () -> execBranchInstr instr
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
  let stoppingCondition = (machineState.Registers.[R15]/4 = List.length (Map.toList machineState.Memory))
  match stoppingCondition with
  | true -> machineState
  | false -> execWrapper (pipeLine machineState)
