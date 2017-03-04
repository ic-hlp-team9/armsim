Load types.fsx

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


type FLexOp =
| Const of Imm8m
| Shift of sbyte*RegisterName

type ArithLogicInstr = {Cond: ConditionCode option; Op: ArithLogicOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FLexOp}
type MoveInstr = {Cond: ConditionCode option; Op: MoveOp; S:bool; Rd: RegisterName; Op2: FLexOp}
type TestInstr = {Cond: ConditionCode option; Op: TestOp; Rn: RegisterName; Op2: FLexOp}
type BranchInstr = {Cond: ConditionCode option; L:bool; Address: byte*uint16} //Address type TBD, 24bit field originally
type MRSInstr = {Cond: ConditionCode option; Rd:RegisterName; Psr:PSR}
type MSRInstr = {Cond: ConditionCode option; Flags: APSRFlag list ;Param: ImReg}
type ShiftInstr = {Cond: ConditionCode option; Op: ShiftOp; S:bool; Rd: RegisterName; Rn: RegisterName; Param: ImReg option} //Last parameter is option becase RRX only has 2 registers as parameters
type MultInstr = {Cond: ConditionCode option; Op: MultOp; S:bool; Rd: RegisterName; Rm: RegisterName; Rs: RegisterName; Rn: RegisterName option} //Mul only has 3 registers as parameters that's why last one is option; MLS cannot have S suffix, therefore it is also option

let pipeLine machineState =
    machineState
    |> fetch
    |> decode
    |> execute

let execute = fun x -> x()

fun () -> executeArithLogicInstr Instr

let rec execWrapper machineState =
    match stopCondition with
    | true -> machineState
    | false -> execWrapper (execute machineState)


let fetchInstr machineState =
    let PC = machineState.registers[R15]
    machineState.memory[PC]


let secondOp flexOp =
    match flexOp with
    | Const n -> n
    | Shift b, Rn -> machineState.registers[Rn] << b


let execMoveInstr movInstr machineState =
    let f =
        match movInstr.Op with
        | MOV -> id
        | MVN (~~~)


let rec execArithLogicInstr arithLogicInstr machineState =
    let opMatch aluOp =
        match aluOp with
        | AND -> (&&&)
        | EOR -> (^^^)
        | SUB -> (+)
        | RSB -> fun a b -> b-a
        | ADD -> fun a b -> a+b
        | ADC -> (+)
        | SBC -> (-)
        | RSC -> fun a b -> b-a
        | ORR -> (|||)
        | BIC -> (~~~)

    let flags =
        match arithLogicInstr.S with
        | false -> machineState.flags

    {machineState with arithLogicInstr.Rd=(opMatch ArithLogicOp)}
