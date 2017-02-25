//let x = 99y
type RegisterName = R0 | R1 | R2 | R3 | R4 | R5| R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

let registerFile = Map [R1, 7 ; R2, 3]

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
| Rot of Nibble*byte
| Shift of byte*RegisterName


type ArithLogicInstr = {Cond: ConditionCode; Op: ArithLogicOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FLexOp}
type MoveInstr = {Cond: ConditionCode; Op: MoveOp; S:bool; Rd: RegisterName; Op2: FLexOp}
type TestInstr = {Cond: ConditionCode; Op: TestOp; S:bool; Rn: RegisterName; Op2: FLexOp}
