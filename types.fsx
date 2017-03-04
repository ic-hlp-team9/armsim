module Types

type Register = int
type Word = uint32

type Instr =
 | ArithLogicInstr // Flex op2
 | MoveInstr // Flex op2
 | TestInstr // Flex op2
 | MultInstr
 | ShiftInstr
 | BranchInstr
 | PSRInstr
 | MemInstr
 | MiscInstr

type ArithLogicInstr = string*int*byte

type PossiblyDecodedWord =
 | Instr
 | Word

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

type MachineRepresentation = {
    Memory: PossiblyDecodedWord list;
    Registers: RegisterFile;
    CPSR: CPSR;
}

// val FrontEnd: string -> (MachineRepresentation Option * FrontendStatusItem list)
