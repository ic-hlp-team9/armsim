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

type Nibble = uint32 //placeholder


type FLexOp =
| Const of Nibble*byte
| Shift of byte*RegisterName


type ArithLogicInstr = {Cond: ConditionCode; Op: ArithLogicOp; S:bool option; Rd: RegisterName; Rn: RegisterName; Op2: FLexOp}
type MoveInstr = {Cond: ConditionCode; Op: MoveOp; S:bool option; Rd: RegisterName; Op2: FLexOp}
type TestInstr = {Cond: ConditionCode; Op: TestOp; Rn: RegisterName; Op2: FLexOp}
type BranchInstr = {Cond: ConditionCode; L:bool; Address: byte*uint16} //Address type

type ShiftOp =
| ASR
| LSL
| LSR
| ROR
| RRX

type ImReg =
| Immediate of int
| Register of RegisterName

type ShiftInstr = {Cond: ConditionCode; Op: ShiftOp; S:bool; Rd: RegisterName; Rn: RegisterName; Param: ImReg option} //Last parameter is option becase RRX only has 2 registers as parameters

type MultOp =
| MUL
| MLA
| MLS
| UMULL
| UMLAL

type MultInstr = {Cond: ConditionCode; Op: MultOp; S:bool option; Rd: RegisterName; Rm: RegisterName; Rs: RegisterName; Rn: RegisterName option} //Mul only has 3 registers as parameters that's why last one is option; MLS cannot have S suffix, therefore it is also option
