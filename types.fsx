type Register = uint32
type Word     = uint32

// Flex Op2

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
    state: FrontendStatus;
    line: uint32 Option;
    char: uint32 Option;
    text: string;
}

type Registers =
 | r1
 | ...

type RegisterFile = // Map <Registers, Register>

type MachineRepresentation = {
    memory: PosisblyDecodedWord list;
    registers: RegisterFile;
}

// val FrontEnd: string -> (MachineRepresentation Option * FrontendStatusItem list)
