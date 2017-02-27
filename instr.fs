//to be merged with types.fsx later
type RegisterName = R0 | R1 | R2 | R3 | R4 | R5| R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

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

type Nibble = uint32 //placeholder
type Imm8m = int //placeholder, should be a number created by rotating an 8-bit value by an even number of bits within a 32-bit register

type ImReg =
| Immediate of Imm8m
| Register of RegisterName


type FLexOp =
| Const of Imm8m
| Shift of byte*RegisterName

type ArithLogicInstr = {Cond: ConditionCode option; Op: ArithLogicOp; S:bool option; Rd: RegisterName; Rn: RegisterName; Op2: FLexOp}
type MoveInstr = {Cond: ConditionCode option; Op: MoveOp; S:bool option; Rd: RegisterName; Op2: FLexOp}
type TestInstr = {Cond: ConditionCode option; Op: TestOp; Rn: RegisterName; Op2: FLexOp}
type BranchInstr = {Cond: ConditionCode option; L:bool; Address: byte*uint16} //Address type TBD, 24bit field originally
type MRSInstr = {Cond: ConditionCode option; Rd:RegisterName; Psr:PSR}
type MSRInstr = {Cond: ConditionCode option; Flags: APSRFlag list ;Param: ImReg}
type ShiftInstr = {Cond: ConditionCode option; Op: ShiftOp; S:bool; Rd: RegisterName; Rn: RegisterName; Param: ImReg option} //Last parameter is option becase RRX only has 2 registers as parameters
type MultInstr = {Cond: ConditionCode option; Op: MultOp; S:bool option; Rd: RegisterName; Rm: RegisterName; Rs: RegisterName; Rn: RegisterName option} //Mul only has 3 registers as parameters that's why last one is option; MLS cannot have S suffix, therefore it is also option
