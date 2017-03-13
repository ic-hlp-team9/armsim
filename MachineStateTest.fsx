type Register = int
type Word = int


type FrontendStatus =
    | Critical of string
    | Warning of string
    | Notice of string
    | Info of string
    | Debug of string


type FrontendStatusItem = {
    State: FrontendStatus;
    Line: uint32 Option;
    Char: uint32 Option;
    Text: string;
}

type RegisterName = R0 | R1 | R2 | R3 | R4 | R5| R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

type RegisterFile =  Map<RegisterName, Register>

type CPSR = {N:bool; Z:bool; C: bool; V: bool}


type ArithLogicOp =
| AND
| EOR
| SUB
| RSB
| ADD
| ADC
| SBC
| RSC
| ORR
| BIC

type MoveOp =
| MOV
| MVN

type TestOp =
| TST
| TEQ
| CMP
| CMN

type PSROp =
| MRS
| MSR

type PSR =
| APSR
| CPSR
| SPSR
| Mpsr

type ConditionCode =
| EQ
| NE
| CS
| CC
| MI
| PL
| VS
| VC
| HI
| LS
| GE
| LT
| GT
| LE
| AL
| NV

type ShiftOp =
| ASR
| LSL
| LSR
| ROR
| RRX

type APSRFlag =
| N
| Z
| C
| V
| Q

type MultOp =
| MUL
| MLA
| MLS
| UMULL
| UMLAL

type Nibble = byte //placeholder
type Imm8m = int //placeholder, should be a number created by rotating an 8-bit value by an even number of bits within a 32-bit register

type ImReg =
| Immediate of Imm8m
| Register of RegisterName


type FlexOp =
| Const of Imm8m
| Shift of sbyte*RegisterName


type ArithLogicInstr = {Cond: ConditionCode option; Op: ArithLogicOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FlexOp}
type MoveInstr = {Cond: ConditionCode option; Op: MoveOp; S:bool; Rd: RegisterName; Op2: FlexOp}
type TestInstr = {Cond: ConditionCode option; Op: TestOp; Rn: RegisterName; Op2: FlexOp}
type BranchInstr = {Cond: ConditionCode option; L:bool; Address: byte*uint16} //Address type TBD, 24bit field originally
type MRSInstr = {Cond: ConditionCode option; Rd:RegisterName; Psr:PSR}
type MSRInstr = {Cond: ConditionCode option; Flags: APSRFlag list ;Param: ImReg}
type ShiftInstr = {Cond: ConditionCode option; Op: ShiftOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FlexOp} //Last parameter is option becase RRX only has 2 registers as parameters
type MultInstr = {Cond: ConditionCode option; Op: MultOp; S:bool; Rd: RegisterName; Rm: RegisterName; Rs: RegisterName; Rn: RegisterName option} //Mul only has 3 registers as parameters that's why last one is option; MLS cannot have S suffix, therefore it is also option


type Instr =
 | ArithLogicInstr of ArithLogicInstr// Flex op2
 | MoveInstr of MoveInstr // Flex op2
 | TestInstr of TestInstr// Flex op2
 | MultInstr of MultInstr
 | ShiftInstr of ShiftInstr
 | BranchInstr of BranchInstr
 | PSRInstr of MRSInstr | MSRInstr
 | MemInstr
 | MiscInstr


type PossiblyDecodedWord =
 | Instr of Instr
 | Word of Word

type MachineRepresentation = {
    Memory: PossiblyDecodedWord list;
    Registers: RegisterFile;
    CPSR: CPSR;
}
// val FrontEnd: string -> (MachineRepresentation Option * FrontendStatusItem list)
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


let myRegs =
   [ R0, 0;
      R1, 0;
      R2, 0;
      R3, 0;
      R4, 0;
      R5, 0;
      R6, 0;
      R7, 0;
      R8, 0;
      R9, 0;
      R10, 0;
      R11, 0;
      R12, 0;
      R13, 0;
      R14, 0;
      R15, 0
      ]
   |> Map.ofList
let myMachineState = { Memory= [ Instr (ArithLogicInstr  {Cond = Some EQ ; Op =  ADD ; S = false ; Rd =  R0; Rn = R1; Op2 = Const 5});
                                 Instr (ArithLogicInstr  {Cond = Some EQ ; Op =  ADD ; S = false ; Rd =  R1; Rn = R0; Op2 = Const 10});
                                 Instr (ArithLogicInstr  {Cond = Some EQ ; Op =  SUB ; S = false ; Rd =  R2; Rn = R1; Op2 = Shift (3y,R0) });
                                 Instr (MoveInstr  {Cond = Some EQ ; Op =  MOV ; S = false ; Rd =  R3; Op2 = Shift (3y,R1)});
                                 Instr (ArithLogicInstr  {Cond = Some EQ ; Op =  RSC ; S = true ; Rd =  R4; Rn = R0; Op2 = Const 5} )] ;
                       Registers = myRegs ;
                       CPSR = {N = false; Z = false; C = false; V = false }
                        }
//let res  =  myMachineState |> pipeLine |> pipeLine |> pipeLine |> pipeLine |> pipeLine
let res = execWrapper myMachineState
//List.length (myMachineState.Memory)
