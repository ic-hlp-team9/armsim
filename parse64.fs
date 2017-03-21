type Register = int
type Word = int
type Address = uint32
type AddressingType = Pre | Post | Offset

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

type ArithOperation = Addition | Subtraction
type ShiftOperation = Left | Right

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


type Token = 
    | TokStrLit of string // string literal labels, with integer values permitted inside
    | TokIntLit of int64 // integer literal 
    | TokOp of string // operators
    | TokDir of string //Used with ldm IA, IB...
    | TokCond of string //condition codes
    | TokReg of string //Register tokens
    | TokComma //Comma used to separate operands/ check syntax
    | TokS // Used for unknown characters
    | TokNeg //Used to signify negative number
    | TokHash //start of digit
    | TokEq // equal sign
    | TokRSbracket | TokLSbracket // right square and left square bracket
    | TokRCbracket | TokLCbracket // right  curly and left curly bracket
    | TokExcl

type DataPInstr = {B:bool; Vals: Token list}
type FillPInstr = {Num: uint32}
type AdrPInstr = {Cond: ConditionCode option; S:bool; Rd: RegisterName; Dest: string}

type PseudoInstr =
    | DataI of DataPInstr
    | Fill of FillPInstr

type ParsedInstr =
    | I of Instr
    | PI of PseudoInstr

//Address type TBD, 24bit field originally

let isDigit (c : char) = List.contains c [ '0'..'9' ]
let isAlpha (c : char) = List.contains c ([ 'a'..'z' ] @ [ 'A'..'Z' ])
let isWhiteSpace (c : char) = List.contains c [ ' '; '\n'; '\t'; '\r'; '\f' ]
let explode (str : string) = str |> List.ofSeq
let implode (x : char list) = x |> System.String.Concat   

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    if f x then incr i
    !i)
  |> Seq.map snd

let explodeByLine (str: string) =
    splitBy ((=) '\n') str
    |> List.ofSeq
    |> List.map List.ofSeq 
    |> List.filter (fun x -> x <> ['\n'])

let opList = [ "AND" ; "EOR" ; "SUB"; "RSB"; "ADD"; "ADC"; "SBC";"RSC"; "ORR"; "BIC"; 
    "MOV"; "MVN"; "TST"; "TEQ"; "CMP"; "CMN"; "ASR"; "LSL"; "LSR"; "ROR"; "RRX"; 
    "MUL"; "MLA"; "MLS"; "UMULL"; "UMLAL"; "SMULL"; "SMLAL"; "BL"; "B" ; "DCD"; "DCB"; "FILL";
    "LDR"; "LDRB"; "STR"; "STRB"; "LDM"; "STM"; "ADR"; "END"]
let multMemList = ["LDM"; "STM"]
let singleMemList = ["LDR"; "LDRB"; "STR"; "STRB"]
let dataList = ["DCD"; "DCB"; "FILL"]
let branList = ["BL"; "B"]
let arithList = ["AND" ; "EOR" ; "SUB"; "RSB"; "ADD"; "ADC"; "SBC";"RSC"; "ORR"; "BIC"]
let moveList = ["MOV"; "MVN"]
let tstList = ["TST"; "TEQ"; "CMP"; "CMN"]
let shiftList = ["ASR"; "LSL"; "LSR"; "ROR"; "RRX"]
let mulList = ["MUL"; "MLA"; "MLS"; "UMULL"; "UMLAL"; "SMULL"; "SMLAL"]
let condList = [ "EQ"; "NE"; "CS"; "CC"; "MI"; "PL"; "VS"; "VC"; "HI"; "HS"; "LO"; "LS"; "GE"; "LT"; "GT"; "LE"; "AL"; "NV"]

let dirList = ["IA"; "IB"; "DA"; "DB"; "ED"; "FD"; "EA"; "FA"]
let regList = [ "R10"; "R11"; "R12"; "R13"; "R14"; "R15"; "R0"; "R1"; "R2"; "R3"; "R4"; "R5"; "R6"; "R7"; "R8"; "R9"]
let rec charListStartsWith (x : char list) (str : string) = 
    let removeFirstChar (s:string) = 
        seq { for n in [1..s.Length-1] do yield str.[n]} 
        |> System.String.Concat
    match x with
    | _ when str = "" -> true // empty string will match anything
    | ch :: r when str.[0] = ch -> charListStartsWith r (removeFirstChar str) // if first char matches check the rest
    | _ -> false // This must be a mismatch

let (|OpMatch|_|) cLst = 
    List.tryFind (charListStartsWith cLst) opList 
    |> Option.map (fun op -> TokOp op, List.skip op.Length cLst)

let (|CondMatch|_|) cLst = 
    List.tryFind (charListStartsWith cLst) condList 
    |> Option.map (fun op -> TokCond op, List.skip op.Length cLst)

let (|DirMatch|_|) cLst = 
    List.tryFind (charListStartsWith cLst) dirList 
    |> Option.map (fun op -> TokDir op, List.skip op.Length cLst)

let (|RegMatch|_|) cLst = 
    List.tryFind (charListStartsWith cLst) regList 
    |> Option.map (fun op -> TokReg op, List.skip op.Length cLst)
let (|IntMatch|_|) cLst =
    let rec iMatch lst =
      match lst with
      | ch :: r when isDigit ch -> ch :: iMatch r
      | _ -> []
    match implode (iMatch cLst) with
    | "" -> None // not an integer
    | s -> Some (TokIntLit (int64 s), List.skip (s.Length) cLst)
let (|StrMatch|_|) cLst =
    let rec sMatch lst =
      match lst with
      | ch :: r when isAlpha ch -> ch :: sMatch r
      | ch :: r when isDigit ch -> ch :: sMatch r
      | _ -> []
    match implode (sMatch cLst) with
    | "" -> None // not a string
    | s -> Some (TokStrLit (s), List.skip (s.Length) cLst)

let tokenise (src:string) = 
    let rec tokenise1 lst =     
        match lst with
        | ch :: r when isWhiteSpace ch-> tokenise1 r
        | [] -> []
        | '#' :: r -> TokHash :: tokenise1 r
        | OpMatch(t, r) ->  t :: tokenise1 r
        | CondMatch (t, r) -> t :: tokenise1 r
        | RegMatch (t, r) -> t :: tokenise1 r
        | IntMatch(t, r) -> t :: tokenise1 r
        | DirMatch (t, r) -> t :: tokenise1 r
        | ',' :: r -> TokComma :: tokenise1 r
        | 'S' :: r -> TokS :: tokenise1 r
        | '=' :: r -> TokEq :: tokenise1 r
        | '-' :: num :: r when isDigit num -> TokNeg :: tokenise1 (num :: r)
        | StrMatch(t, r) -> t :: tokenise1 r
        | '[' :: r -> TokLSbracket :: tokenise1 r
        | ']' :: r -> TokRSbracket :: tokenise1 r
        | '{' :: r -> TokLCbracket :: tokenise1 r
        | '}' :: r -> TokRCbracket :: tokenise1 r
        | '!' :: r -> TokExcl :: tokenise1 r
        | _ -> failwith "Syntax error: Invalid character used in the input, check your code"
    src.ToUpper()
    |> explodeByLine
    |> List.map tokenise1

//type ArithLogicInstr = {Cond: ConditionCode option; Op: ArithLogicOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FlexOp}
let toReg (r:string):RegisterName =
    match r with
    | "R0" -> R0
    | "R1" -> R1
    | "R2" -> R2
    | "R3" -> R3
    | "R4" -> R4
    | "R5" -> R5
    | "R6" -> R6
    | "R7" -> R7
    | "R8" -> R8
    | "R9" -> R9
    | "R10" -> R10
    | "R11" -> R11
    | "R12" -> R12
    | "R13" -> R13
    | "R14" -> R14
    | _ -> R15
    
//let condList = [ "EQ"; "NE"; "CS"; "CC"; "MI"; "PL"; "VS"; "VC"; "HI"; "LS"; "GE"; "LT"; "GT"; "LE"; "AL"; "NV"]
let toCond (c:string):ConditionCode =
    match c with
    | "EQ" -> EQ
    | "NE" -> NE
    | "HS" -> CS
    | "LO" -> CC
    | "CS" -> CS
    | "CC" -> CC
    | "MI" -> MI
    | "PL" -> PL
    | "VS" -> VS
    | "VC" -> VC
    | "HI" -> HI
    | "LS" -> LS
    | "GE" -> GE
    | "LT" -> LT
    | "GT" -> GT
    | "LE" -> LE
    | "AL" -> AL
    | _ -> NV
//let arithList = ["AND" ; "EOR" ; "SUB"; "RSB"; "ADD"; "ADC"; "SBC";"RSC"; "ORR"; "BIC"]
let toAInstr (i:string):ArithLogicOp =
    match i with
    | "AND" -> AND
    | "EOR" -> EOR
    | "SUB" -> SUB
    | "RSB" -> RSB
    | "ADD" -> ADD
    | "ADC" -> ADC
    | "SBC" -> SBC
    | "RSC" -> RSC
    | "ORR" -> ORR
    | _ -> BIC

let toMInstr (i:string):MoveOp =
    match i with
    | "MOV" -> MOV
    | _ -> MVN

let toTInstr (i:string):TestOp =
    match i with
    | "TST" -> TST
    | "TEQ" -> TEQ
    | "CMP" -> CMP
    | _ -> CMN

let toSInstr (i:string):ShiftOp =
    match i with
    | "ASR" -> ASR
    | "LSL" -> LSL
    | "LSR" -> LSR
    | "ROR" -> ROR
    | _ -> RRX

let toMulInstr (i:string):MultOp =
    match i with
    | "MUL" -> MUL
    | "MLA" -> MLA
    | "MLS" -> MLS
    | "UMULL" -> UMULL
    | "UMLAL" -> UMLAL
    | "SMULL" -> SMULL
    | _ -> SMLAL

let toDir i (isLoad:bool):Dir =
    match i with
    | "IA" -> IA
    | "IB" -> IB
    | "DA" -> DA
    | "DB" -> DB
    | "ED" -> if isLoad then IB else DA
    | "EA" -> if isLoad then DB else IA
    | "FD" -> if isLoad then IA else DB
    | _ -> if isLoad then DA else IB
let matchFlexNew t =
    match t with
        | [TokReg r1] -> Shift(LSL, Immediate 0 ,(toReg r1))
        | TokReg r1 :: TokComma :: TokOp sh :: TokHash :: [TokIntLit x] when (List.exists (fun elem -> elem = sh) shiftList) && sh <> "RRX" -> Shift(toSInstr sh ,Immediate (int x), (toReg r1))
        | TokReg r1 :: TokComma :: TokOp sh :: [TokReg r2] when (List.exists (fun elem -> elem = sh) shiftList) && sh <> "RRX" -> Shift(toSInstr sh , Register (toReg r2), (toReg r1))
        | TokReg r1 :: TokComma :: [TokOp sh] when sh = "RRX" -> Shift(toSInstr sh, Immediate 0, (toReg r1))
        | TokHash :: [TokIntLit x] -> (Const (int x))
        | TokHash :: TokNeg :: [TokIntLit x] -> (Const (int -x))
        | _ -> failwith "Syntax error: Flex operand has invalid syntax, flex op is used at the end of arithmetic, move and test instruction"

let parseArithInstr tokList =
    let mutable aInst:ArithLogicInstr = {Op = AND; S = false; Rd = R0; Rn = R0; Op2 = Shift (LSL, Immediate 0, R0)}
    let mutable cond = None;
    let matchFlex t =
        (cond, ArithLogicInstr {aInst with Op2 = matchFlexNew t})
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: TokReg r2 :: TokComma :: r -> (aInst <- {aInst with Rd = toReg r1; Rn = toReg r2}); (matchFlex r)
        | _ -> failwith "Syntax error: Check your arithmetic instruction, possible comma missing"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (aInst <- {aInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (aInst <- {aInst with Op = toAInstr op}); (matchS t)
    | _ -> failwith "Syntax error: Check your arithmetic instruction"


let parseMoveInstr tokList =
    let mutable mInst:MoveInstr = {Op = MOV; S = false; Rd = R0; Op2 = Shift (LSL, Immediate 0, R0)}
    let mutable cond = None;
    let matchFlex t =
        (cond, MoveInstr {mInst with Op2 = matchFlexNew t})
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: r -> (mInst <- {mInst with Rd = toReg r1}); (matchFlex r)
        | _ -> failwith "Syntax error: Check your move instruction, possible comma missing"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (mInst <- {mInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (mInst <- {mInst with Op = toMInstr op}); (matchS t)
    | _ -> failwith "Syntax error: Check your move instruction"

let parseTestInstr tokList =
    let mutable tInst:TestInstr = {Op = TST; Rn = R0; Op2 = Shift (LSL,Immediate 0, R0)}
    let mutable cond = None;
    let matchFlex t =
        (cond, TestInstr {tInst with Op2 = matchFlexNew t})
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: r -> (tInst <- {tInst with Rn = toReg r1}); (matchFlex r)
        | _ -> failwith "Syntax error: Check your test instruction, possible comma missing"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchRegs r)
        | r -> matchRegs r

    match tokList with
    | TokOp op :: t -> (tInst <- {tInst with Op = toTInstr op}); (matchCond t)
    | _ -> failwith "Syntax error: Check your test instruction"

let parseShiftInstr tokList =
    let mutable sInst:ShiftInstr = {Op = LSR; S = false; Rd = R0; Rn = R0; Op2 = Register R0}
    let mutable cond = None;
    let matchImReg t =
        match t with
        | TokComma :: [TokReg r1] when sInst.Op <> RRX -> (cond, ShiftInstr {sInst with Op2 = Register (toReg r1)})
        | TokComma :: TokHash :: [TokIntLit x] when sInst.Op <> RRX && x<256L -> (cond, ShiftInstr {sInst with Op2 = Immediate (int x)})
        | TokComma :: TokHash :: TokNeg :: [TokIntLit x] when sInst.Op <> RRX -> failwith "Value error: Shift value cannot be negative"
        | [] when sInst.Op = RRX -> (cond, ShiftInstr {sInst with Op2 = Immediate 0})
        | _ -> failwith "Syntax error: Check your shift instruction, remember special case for RRX and maximum value of immediate"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: TokReg r2 :: r -> (sInst <- {sInst with Rd = toReg r1; Rn = toReg r2}); (matchImReg r)
        | _ -> failwith "Syntax error: Check your shift instruction, possible comma missing"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (sInst <- {sInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (sInst <- {sInst with Op = toSInstr op}); (matchS t)
    | _ -> failwith "Syntax error: Check your shift instruction"

let parseMulInstr tokList =
    let mutable mulInst:MultInstr = {Op = MUL; S=false; Rd=R0; Rm=R0; Rs=R0; Rn = None}
    let mutable cond = None;
    let matchLast t =
        match t with
        | TokComma :: [TokReg r1] when mulInst.Op <> MUL -> (cond, MultInstr {mulInst with Rn = Some (toReg r1)})
        | [] when mulInst.Op = MUL -> (cond, MultInstr mulInst)
        | _ -> failwith "Syntax error: Check your multiplication instruction, remember special case for MUL"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: TokReg r2 :: TokComma :: TokReg r3 :: r -> (mulInst <- {mulInst with Rd = toReg r1; Rm = toReg r2; Rs = toReg r3}); (matchLast r)
        | _ -> failwith "Syntax error: Check your multiplication instruction, possible comma missing"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (mulInst <- {mulInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (mulInst <- {mulInst with Op = toMulInstr op}); (matchS t)
    | _ -> failwith "Syntax error: Check your multiplication instruction"

//type PreAssembleBI = {Cond: ConditionCode option; L:bool; Dest: string} 
let parseBranInstr tokList =
    let mutable brInst = {L = false; Dest = ""}
    let mutable cond = None;
    let matchDest t =
        match t with
        | [TokStrLit x] -> (cond, PreAssembleBI {brInst with Dest = x})
        | _ -> failwith "Syntax error: Check your branch instruction syntax, possible uneccessary elements at the end"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchDest r)
        | r -> matchDest r
    match tokList with
    | TokOp "BL" :: t -> (brInst <- {brInst with L = true}); (matchCond t)
    | TokOp "B" :: t -> matchCond t
    | _ -> failwith "Syntax error: Check your branch instruction syntax"

let parseDataInstr tokList:PseudoInstr =
    match tokList with
    | TokOp "DCD" :: r -> DataI {B = false; Vals = r}
    | TokOp "DCB" :: r -> DataI {B = true; Vals = r}
    | TokOp "FILL" :: [TokIntLit x] when (x%4L) = 0L -> Fill {Num = uint32 x}
    | _ -> failwith "Value error: FILL instruction have to have value divisible by 4"

//type SingleMemInstr = {Op: SingleMemOp; Addressing: AddressingType; ByteAddressing: bool; Pointer: RegisterName; Rd: RegisterName; Offset: FlexOp}
let parseSingleMemInstr tokList =
    let mutable smInstr:SingleMemInstr = {Op = LDR; Addressing = Offset; ByteAddressing = false; Pointer = R0; Rd = R0; Offset = Const 0}
    let mutable cond = None
    let checkExcl t =
        match t with
        | [TokExcl] -> (cond, MemInstr (SingleMemInstr {smInstr with Addressing = Pre}))
        | [] -> (cond, MemInstr (SingleMemInstr smInstr))
        | _ -> failwith "Syntax error: Correct your memory instruction, unnecessary characters at the end"
    let matchFlexInter t =
        match t with
        | TokReg r1 :: TokRSbracket :: r -> smInstr <- {smInstr with Offset = Shift(LSL, Immediate 0 ,(toReg r1))}; checkExcl r
        | TokReg r1 :: TokComma :: TokOp sh :: TokHash :: TokIntLit x :: TokRSbracket :: r when (List.exists (fun elem -> elem = sh) shiftList) -> smInstr <- {smInstr with Offset = Shift(toSInstr sh ,Immediate (int x), (toReg r1))}; checkExcl r
        | TokReg r1 :: TokComma :: TokOp sh :: TokReg r2 :: TokRSbracket :: r when (List.exists (fun elem -> elem = sh) shiftList) -> smInstr <- {smInstr with Offset = Shift(toSInstr sh , Register (toReg r2), (toReg r1))}; checkExcl r
        | TokHash :: TokIntLit x :: TokRSbracket :: r -> smInstr <- {smInstr with Offset = (Const (int x))}; checkExcl r
        | TokHash :: TokNeg :: TokIntLit x :: TokRSbracket :: r -> smInstr <- {smInstr with Offset = (Const (int -x))}; checkExcl r
        | _ -> failwith "Syntax error: Correct your memory instruction, possible wrong bracket or comma position"
    let matchLast t =
        match t with
        | TokLSbracket :: TokReg r1 :: [TokRSbracket] -> (cond, MemInstr (SingleMemInstr {smInstr with Pointer = toReg r1}))
        | TokLSbracket :: TokReg r1 :: TokRSbracket :: TokComma :: r -> (cond, MemInstr (SingleMemInstr {smInstr with Pointer = toReg r1; Offset = matchFlexNew r; Addressing = Post}))
        | TokLSbracket :: TokReg r1 :: TokComma :: r -> smInstr <- {smInstr with Pointer = toReg r1}; matchFlexInter r
        | _ -> failwith "Syntax error: Correct your memory instruction, possible wrong bracket or comma position"
    let matchReg t =
        match t with
        | TokReg r1 :: TokComma :: r -> (smInstr <- {smInstr with Rd = toReg r1}); matchLast r
        | _ -> failwith "Syntax error: Correct your memory instruction, possible comma missing"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchReg r)
        | r -> matchReg r
    match tokList with
    | TokOp "LDR" :: r -> matchCond r
    | TokOp "LDRB" :: r -> (smInstr <- {smInstr with ByteAddressing = true}); matchCond r
    | TokOp "STR" :: r -> (smInstr <- {smInstr with Op = STR}); matchCond r
    | TokOp "STRB" :: r -> (smInstr <- {smInstr with Op = STR; ByteAddressing = true}); matchCond r
    | _ -> failwith "Not possible"

//type MultiMemInstr = {Op: MultMemOp; Dir: Dir; Pointer: RegisterName; Rlist: RegisterName list; WriteBack: bool}
let parseMultMemInstr tokList =
    let mutable mmInstr:MultiMemInstr = {Op = LDM; Dir = DB; Pointer = R0; Rlist = []; WriteBack = false}
    let mutable cond = None
    let makeListRegs tokenList =
        let rec checkSyntax tokList =
            match tokList with
            | TokLCbracket :: TokReg r1 :: [TokRCbracket] -> true
            | TokLCbracket :: TokReg r1 :: TokComma :: r -> checkSyntax r
            | TokReg r1 :: TokComma :: r -> checkSyntax r
            | TokReg r1 :: [TokRCbracket] -> true
            | _ -> false
        
        if checkSyntax tokenList then
            let createRegList acc elem =
                match elem with
                | TokReg r1 -> toReg r1 :: acc
                | _ -> acc
            List.fold createRegList [] tokenList 
        else
            failwith "Syntax error: The list of registers for LDM/STM has wrong format, or contains forbidden register"
    let matchPointer t =
        match t with
        | TokReg r1 :: TokComma :: r when r1 <> "R15"-> (cond, MemInstr (MultiMemInstr {mmInstr with Pointer = toReg r1; Rlist = (makeListRegs r)}))
        | TokReg r1 :: TokExcl :: TokComma :: r when r1 <> "R15" -> (cond, MemInstr (MultiMemInstr {mmInstr with Pointer = toReg r1; WriteBack = true; Rlist = (makeListRegs r)}))
        | _ -> failwith "Syntax error: Wrong format of LDM/STM instruction, possible comma missing or you defined PC as pointer"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchPointer r)
        | r -> matchPointer r
    match tokList with
    | TokOp "LDM" :: TokDir d :: r -> mmInstr <- {mmInstr with Dir = (toDir d true)}; matchCond r
    | TokOp "STM" :: TokDir d :: r -> mmInstr <- {mmInstr with Op = STM; Dir = (toDir d false)}; matchCond r
    | _ -> failwith "Syntax error: Wrong format of LDM/STM instruction, check if you specify the stack direction"

let parseAddrLoad tokList =
    let mutable cond = None
    let mutable isAdr = false
    let matchRest t =
        match t with 
        | TokReg r1 :: TokComma :: [TokStrLit str] when isAdr ->  (cond, PreAssembleAL {Rd = toReg r1; Address = str})
        | TokReg r1 :: TokComma :: TokEq :: [TokStrLit str] when (not isAdr) -> (cond, PreAssembleAL {Rd = toReg r1; Address = str})
        | _ -> failwith "Syntax error: Address loading instructions (ADR/LDR) have wrong format"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  cond <- Some (toCond cnd); (matchRest r)
        | r -> matchRest r
    match tokList with
    | TokOp "ADR" :: r -> isAdr <- true; matchCond r
    | TokOp "LDR" :: r -> matchCond r
    | _ -> failwith "Not possible" //To get rid of warning 
let parseInstr (tokList: Token list):ParsedInstr =
    match tokList with
    | [TokOp "END"] -> I (None,EndInstr)
    | TokOp "ADR" :: r -> I (parseAddrLoad tokList)
    | TokOp "LDR" :: r when (List.exists (fun elem -> elem = TokEq) r) -> I (parseAddrLoad tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) arithList) -> I (parseArithInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) moveList) -> I (parseMoveInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) tstList) -> I (parseTestInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) shiftList) -> I (parseShiftInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) mulList) -> I (parseMulInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) branList) -> I (parseBranInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) singleMemList) -> I ((parseSingleMemInstr tokList))
    | TokOp x :: r when (List.exists (fun elem -> elem = x) multMemList) -> I ((parseMultMemInstr tokList))
    | TokOp x :: r when (List.exists (fun elem -> elem = x) dataList) -> PI (parseDataInstr tokList)
    | _ -> failwith "Syntax error: Trying to execute unknown/unimplemented instruction, or your END instruction has wrong format"


//(Instr option)*(string option) list
let createInstList (tokList:Token list list):(ParsedInstr * string option) list =
    let preassmeblerInstrList (tList:Token list): (ParsedInstr)*(string option) =
        match tList with
        | TokStrLit s :: TokOp x :: r -> (parseInstr (TokOp x :: r ), Some (s))
        | TokOp x :: r -> (parseInstr (TokOp x :: r ), None)
        //| [TokStrLit s] -> (None, Some s) //SPecial case, label is on separate line
        | _ -> failwith "Syntax error: Please put the label on the same line as instruction"
    
    List.filter (fun x -> x<>[]) tokList
    |> List.map preassmeblerInstrList
    

let myRegs =
   [ R0, 0;  R1, 0;  R2, 0; R3, 0; R4, 0; R5, 0;  R6, 0; R7, 0; R8, 0; R9, 0; R10, 0; R11, 0; R12, 0; R13, 0xFF000000;  R14, 0; R15, 8]
   |> Map.ofList

let doAssembler (iList: (ParsedInstr * string option) list):MachineRepresentation = 
    let mutable progMap = Map.empty
    let init = {
        Memory = Map.empty;
        Registers = myRegs;
        CPSR = {N = false; Z = false; C = false; V = false }
        DataPointer = 0x100u;
        }
        
    let checkPseudo (machState, pointer) elem =       
        let addData machS p d =
            let rec createNumLst finLis tkLis =
                match tkLis with
                | TokNeg :: TokIntLit x :: TokComma :: r -> createNumLst (-x :: finLis) r
                | TokIntLit x :: TokComma :: r -> createNumLst (x :: finLis) r
                | TokNeg :: [TokIntLit x] -> -x :: finLis
                | [TokIntLit x] -> x :: finLis
                | _ -> failwith "Syntax error: wrong syntax used for DCD/DCB, check if values are comma separated"
            let doWordMemWrite mS po nLst =
                List.fold (fun acc elem -> {(fst acc) with Memory = Map.add (snd acc) (Word elem) (fst acc).Memory}, (snd acc)+4u) (mS, po) (List.rev nLst)
            let doByteMemWrite mS po nLst =
                let rec createWordsFromBytes  = function
                    | a :: b :: c :: d :: t -> a + (b <<< 8) + (c <<< 16) + (d<<<24) :: createWordsFromBytes t
                    | a :: b :: [c] -> [a + (b <<< 8) + (c <<< 16)]
                    | a :: [b] ->  [a + (b <<< 8)]
                    | [a] -> [a]
                    | [] -> []
                let checkBytes x =
                    match x with
                    | num when num > 255 || num < - 128 -> failwith "Value error: When defining bytes, make sure values are not bigger than 8 bits"
                    | num -> num 
                List.map checkBytes (List.rev nLst)
                |> createWordsFromBytes
                |> List.rev
                |> doWordMemWrite mS po
            
            let numList = (createNumLst [] d.Vals)
            match d.B with
            | true -> doByteMemWrite machS p  (List. map (int) numList)
            | false -> doWordMemWrite machS p (List. map (int) numList)
           
        
        let addEmpty machS p (num:uint32) =
           
            List.fold (fun acc elem -> {(fst acc) with Memory = Map.add (snd acc) (Word 0) (fst acc).Memory}, (snd acc)+4u) (machS, p) [1..int(num/4u)]

        match elem with
        | (PI (DataI x), Some str) -> progMap <- progMap.Add(str, pointer); (addData machState pointer x)
        | (PI (DataI x), None) -> failwith "Syntax error: Put label in front of DCD/DCB"
        | (PI (Fill x), Some str) -> progMap <- progMap.Add(str, pointer); addEmpty machState pointer x.Num 
        | (PI (Fill x), None) -> addEmpty machState pointer x.Num
        | (I x, _) -> (machState, pointer)
       // | _ -> failwith "Unimplemented pseudo operand"
 
    
    let assembleBranch (cnd,(preBr:PreAssembleBI)):Instr =
        let mutable brInst:BranchInstr = {L = false; Address = 0u}
        match preBr.Dest with
        | x when (Map.containsKey x progMap) -> (cnd, BranchInstr {brInst with L = preBr.L; Address = (Map.find x progMap)})
        | _ -> failwith "Assembler error: Branching to unknown label"
    let assembleAddresLoad (cnd, (preAL:PreAssembleAL)):Instr =
        match preAL.Address with
        | x when (Map.containsKey x progMap) -> (cnd, MoveInstr {Op = MOV; S = false; Rd = preAL.Rd; Op2 = Const (int (Map.find x progMap))})
        | _ -> failwith "Assembler error: Loading address of unknown label"
    let mutable c = 0u
    let addLabels elem =
        match elem with 
        | (instr, Some str) -> progMap <- progMap.Add(str, c); c <- c+4u; elem
        | (instr, _ ) -> c <- c+4u; elem

    let addInstruction (machState, count:Address) (elem:(Instr * string option)) =
        //let count:uint32 = 0u
        let addIn count (elem:(Instr * string option)) =
            match elem with       
            | (x, PreAssembleAL instr), _  -> machState.Memory.Add(count, Instr (assembleAddresLoad (x,instr)))          
            | (x, PreAssembleBI instr), _  -> machState.Memory.Add(count, Instr (assembleBranch (x,instr)))            
            | (instr, _ ) -> machState.Memory.Add(count, Instr instr)       
        
        ({machState with Memory = (addIn count elem)}, count + 4u)
    
    //check length of input code and set DAta pointer accordingly
    let updated = {init with DataPointer = uint32 (256 * int (((iList.Length*4)/256)+1))}
    //COntinue code as normal
    let final = fst(List.fold checkPseudo (updated, updated.DataPointer) iList)

    let transformList elem =
        match elem with
        | (I inst), Some str -> (inst, Some str)
        | (I inst), None -> (inst, None)
        | _ -> failwith "Something went horribly wrong"
    
    let removePI elem =
        match elem with
        | (I inst), _ -> true
        | _ -> false

    List.filter removePI iList
    |> List.map (transformList >> addLabels)
    |> List.fold addInstruction (final, 0u) //iList
    |> fst
    
    
let programASM = "
dta DCD 10,50,154252,53,0,1,15,1000,213,12
LDR R0, =dta
LDMFD R0!, {R1,R2,R3,R4,R5}
ADCS R8, R3, R10, ROR R11
"
programASM
|> tokenise
|> createInstList
|> doAssembler

//|> List.


(*

let final = {
        Memory = [];
        Registers = myRegs;
        CPSR = {N = false; Z = false; C = false; V = false }
        }
    final

*)