module types

type Register = int
type Word = int
type Address = uint32
type AddressingType = Pre | Post | Offset
type ArithOperation = Addition | Subtraction
type ShiftOperation = Left | Right


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
| SMULL
| SMLAL

type SingleMemOp =
| LDR
| STR

type MultMemOp =
| LDM
| STM

type Dir =
| IA
| IB
| DA
| DB

type Nibble = byte //placeholder
type Imm8m = int //placeholder, should be a number created by rotating an 8-bit value by an even number of bits within a 32-bit register


type ImReg =
| Immediate of Imm8m
| Register of RegisterName


type FlexOp =
| Const of Imm8m
| Shift of ShiftOp*ImReg*RegisterName


type ArithLogicInstr = {Op: ArithLogicOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FlexOp}
type MoveInstr = {Op: MoveOp; S:bool; Rd: RegisterName; Op2: FlexOp}
type TestInstr = {Op: TestOp; Rn: RegisterName; Op2: FlexOp}
type BranchInstr = {L:bool; Address: Address} //Address type TBD, 24bit field originally
type PreAssembleBI = {L:bool; Dest: string}
type PreAssembleAL = {Rd: RegisterName; Address:string}
type ShiftInstr = {Op: ShiftOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: ImReg} //Last parameter is option becase RRX only has 2 registers as parameters
type MultInstr = {Op: MultOp; S:bool; Rd: RegisterName; Rm: RegisterName; Rs: RegisterName; Rn: RegisterName option} //Mul only has 3 registers as parameters that's why last one is option; MLS cannot have S suffix, therefore it is also option
type SingleMemInstr = {Op: SingleMemOp; Addressing: AddressingType; ByteAddressing: bool; Pointer: RegisterName; Rd: RegisterName; Offset: FlexOp}
type MultiMemInstr = {Op: MultMemOp; Dir: Dir; Pointer: RegisterName; Rlist: RegisterName list; WriteBack: bool}
type MemInstr =
| SingleMemInstr of SingleMemInstr
| MultiMemInstr of MultiMemInstr


type InstrType =
 | ArithLogicInstr of ArithLogicInstr// Flex op2
 | MoveInstr of MoveInstr // Flex op2
 | TestInstr of TestInstr// Flex op2
 | MultInstr of MultInstr
 | ShiftInstr of ShiftInstr
 | BranchInstr of BranchInstr
 | PreAssembleBI of PreAssembleBI
 | PreAssembleAL of PreAssembleAL
 | MemInstr of MemInstr
 | EndInstr

type Instr = ConditionCode option*InstrType

type PossiblyDecodedWord =
 | Instr of Instr
 | Word of Word

type MachineRepresentation = {
    Memory: Map<Address, PossiblyDecodedWord>;
    Registers: RegisterFile;
    CPSR: CPSR;
    DataPointer: Address;
}
// val FrontEnd: string -> (MachineRepresentation Option * FrontendStatusItem list)
