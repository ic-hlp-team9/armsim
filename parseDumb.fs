open System.Collections.Generic

type Register = int
type Word = int
type Address = uint32


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
    Memory: Map<Address, PossiblyDecodedWord>;
    Registers: RegisterFile;
    CPSR: CPSR;
}

type Token = 
    | TokStrLit of string // string literal "Hello world"
    | TokIntLit of int // integer literal 1234 (only allow positive literals but unary - can be used to make them negative)
    | TokOp of string // operators like "+", "*", "::", "NOT" but also keywords like "LET", "IF".
    | TokCond of string //condition codes
    | TokReg of string
    | TokComma //Comma used to separate operands/ check syntax
    | TokS // Used for unknown characters
    | TokNeg //Used to signify negative number
    | TokUnk


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
    "MUL"; "MLA"; "MLS"; "UMULL"; "UMLAL"; "SMULL"; "SMLAL" ]

let arithList = ["AND" ; "EOR" ; "SUB"; "RSB"; "ADD"; "ADC"; "SBC";"RSC"; "ORR"; "BIC"]
let moveList = ["MOV"; "MVN"]
let testList = ["TST"; "TEQ"; "CMP"; "CMN"]
let shiftList = ["ASR"; "LSL"; "LSR"; "ROR"; "RRX"]
let mulList = ["MUL"; "MLA"; "MLS"; "UMULL"; "UMLAL"; "SMULL"; "SMLAL"]
let condList = [ "EQ"; "NE"; "CS"; "CC"; "MI"; "PL"; "VS"; "VC"; "HI"; "LS"; "GE"; "LT"; "GT"; "LE"; "AL"; "NV"]

let regList = [ "R0"; "R1"; "R2"; "R3"; "R4"; "R5"; "R6"; "R7"; "R8"; "R9"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15"]
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
    | s -> Some (TokIntLit (int s), List.skip (s.Length) cLst)
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
        | '#' :: r -> tokenise1 r
        | OpMatch(t, r) ->  t :: tokenise1 r
        | CondMatch (t, r) -> t :: tokenise1 r
        | RegMatch (t, r) -> t :: tokenise1 r
        | IntMatch(t, r) -> t :: tokenise1 r
        | ',' :: r -> TokComma :: tokenise1 r
        | 'S' :: r -> TokS :: tokenise1 r
        | '-' :: num :: r when isDigit num -> TokNeg :: tokenise1 (num :: r)
        | StrMatch(t, r) -> t :: tokenise1 r
        | ch :: r -> TokUnk :: tokenise1 r  //Uknown character, add unknow token to the list 
    src
    |> explodeByLine
    |> List.map tokenise1


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

let parseArithInstr tokList:ArithLogicInstr =
    let mutable aInst:ArithLogicInstr = {Cond = None; Op = AND; S = false; Rd = R0; Rn = R0; Op2 = Shift (0y, R0)}
    let matchFlex t =
        match t with
        | [TokReg r1] -> {aInst with Op2 = Shift(0y,(toReg r1))}
        | TokReg r1 :: TokComma :: TokOp "LSL" :: [TokIntLit x] -> {aInst with Op2 = Shift(sbyte x, (toReg r1))}
        | TokReg r1 :: TokComma :: TokOp "LSR" :: [TokIntLit x] -> {aInst with Op2 = Shift(sbyte (-x), (toReg r1))}
        | [TokIntLit x] -> {aInst with Op2 = Const x}
        | TokNeg :: [TokIntLit x] -> {aInst with Op2 = Const -x}
        | _ -> failwithf "Invalid syntax"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: TokReg r2 :: TokComma :: r -> (aInst <- {aInst with Rd = toReg r1; Rn = toReg r2}); (matchFlex r)
        | _ -> failwithf "Invalid syntax"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  (aInst <- {aInst with Cond = Some (toCond cnd)}); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (aInst <- {aInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (aInst <- {aInst with Op = toAInstr op}); (matchS t)
    | _ -> failwithf "Invalid syntax"


let parseMoveInstr tokList:MoveInstr =
    let mutable mInst:MoveInstr = {Cond = None; Op = MOV; S = false; Rd = R0; Op2 = Shift (0y, R0)}
    let matchFlex t =
        match t with
        | [TokReg r1] -> {mInst with Op2 = Shift(0y,(toReg r1))}
        | TokReg r1 :: TokComma :: TokOp "LSL" :: [TokIntLit x] -> {mInst with Op2 = Shift(sbyte x, (toReg r1))}
        | TokReg r1 :: TokComma :: TokOp "LSR" :: [TokIntLit x] -> {mInst with Op2 = Shift(sbyte (-x), (toReg r1))}
        | [TokIntLit x] -> {mInst with Op2 = Const x}
        | TokNeg :: [TokIntLit x] -> {mInst with Op2 = Const -x}
        | _ -> failwithf "Invalid syntax"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: r -> (mInst <- {mInst with Rd = toReg r1}); (matchFlex r)
        | _ -> failwithf "Invalid syntax"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  (mInst <- {mInst with Cond = Some (toCond cnd)}); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (mInst <- {mInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (mInst <- {mInst with Op = toMInstr op}); (matchS t)
    | _ -> failwithf "Invalid syntax"

let parseTestInstr tokList:TestInstr =
    let mutable tInst:TestInstr = {Cond = None; Op = TST; Rn = R0; Op2 = Shift (0y, R0)}
    let matchFlex t =
        match t with
        | [TokReg r1] -> {tInst with Op2 = Shift(0y,(toReg r1))}
        | TokReg r1 :: TokComma :: TokOp "LSL" :: [TokIntLit x] -> {tInst with Op2 = Shift(sbyte x, (toReg r1))}
        | TokReg r1 :: TokComma :: TokOp "LSR" :: [TokIntLit x] -> {tInst with Op2 = Shift(sbyte (-x), (toReg r1))}
        | [TokIntLit x] -> {tInst with Op2 = Const x}
        | TokNeg :: [TokIntLit x] -> {tInst with Op2 = Const -x}
        | _ -> failwithf "Invalid syntax"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: r -> (tInst <- {tInst with Rn = toReg r1}); (matchFlex r)
        | _ -> failwithf "Invalid syntax"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  (tInst <- {tInst with Cond = Some (toCond cnd)}); (matchRegs r)
        | r -> matchRegs r

    match tokList with
    | TokOp op :: t -> (tInst <- {tInst with Op = toTInstr op}); (matchCond t)
    | _ -> failwithf "Invalid syntax"

let parseShiftInstr tokList:ShiftInstr =
    let mutable sInst:ShiftInstr = {Cond = None; Op = LSR; S = false; Rd = R0; Rn = R0; Op2 = Shift (0y, R0)}
    let matchFlex t =
        match t with
        | [TokReg r1] when sInst.Op <> RRX -> {sInst with Op2 = Shift(0y,(toReg r1))}
        | [TokIntLit x] when sInst.Op <> RRX -> {sInst with Op2 = Const x}
        | TokNeg :: [TokIntLit x] when sInst.Op <> RRX -> {sInst with Op2 = Const -x}
        | [] when sInst.Op = RRX -> {sInst with Op2 = Const 0}
        | _ -> failwithf "Invalid syntax"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: TokReg r2 :: TokComma :: r -> (sInst <- {sInst with Rd = toReg r1; Rn = toReg r2}); (matchFlex r)
        | _ -> failwithf "Invalid syntax"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  (sInst <- {sInst with Cond = Some (toCond cnd)}); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (sInst <- {sInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (sInst <- {sInst with Op = toSInstr op}); (matchS t)
    | _ -> failwithf "Invalid syntax"

let parseMulInstr tokList:MultInstr =
    let mutable mulInst = {Cond = None; Op = MUL; S=false; Rd=R0; Rm=R0; Rs=R0; Rn = None}
    let matchLast t =
        match t with
        | TokComma :: [TokReg r1] when mulInst.Op <> MUL -> {mulInst with Rn = Some (toReg r1)}
        | [] when mulInst.Op = MUL -> mulInst
        | _ -> failwithf "Invalid syntax"
    let matchRegs t =
        match t with
        | TokReg r1 :: TokComma :: TokReg r2 :: TokComma :: TokReg r3 :: r -> (mulInst <- {mulInst with Rd = toReg r1; Rm = toReg r2; Rs = toReg r3}); (matchLast r)
        | _ -> failwithf "Invalid syntax"
    let matchCond t =
        match t with
        | TokCond cnd :: r ->  (mulInst <- {mulInst with Cond = Some (toCond cnd)}); (matchRegs r)
        | r -> matchRegs r
    let matchS t =
        match t with
        | TokS :: r -> (mulInst <- {mulInst with S = true}); (matchCond r)
        | r -> matchCond r 

    match tokList with
    | TokOp op :: t -> (mulInst <- {mulInst with Op = toMulInstr op}); (matchS t)
    | _ -> failwithf "Invalid syntax"

let parseInstr (tokList: Token list):Instr =
    match tokList with
    | TokOp x :: r when (List.exists (fun elem -> elem = x) arithList) -> ArithLogicInstr (parseArithInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) moveList) -> MoveInstr (parseMoveInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) testList) -> TestInstr (parseTestInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) shiftList) -> ShiftInstr (parseShiftInstr tokList)
    | TokOp x :: r when (List.exists (fun elem -> elem = x) mulList) -> MultInstr (parseMulInstr tokList)
    | _ -> failwithf "Unimplmeneted operand"


//(Instr option)*(string option) list
let createInstList (tokList:Token list list):(Instr option * string option) list =
    let preassmeblerInstrList (tList:Token list): (Instr option)*(string option) =
        match tList with
        | TokStrLit s :: TokOp x :: r -> (Some (parseInstr (TokOp x :: r )), Some (s))
        | [TokStrLit s] -> (None, Some s) //SPecial case, label is on separate line
        | TokOp x :: r -> (Some (parseInstr (TokOp x :: r )), None)
        | _ -> failwithf "Invalid syntax"
    
    List.map preassmeblerInstrList tokList
    

let doAssembler (iList: (Instr option * string option) list):MachineRepresentation = 
    let final = {
        Memory = Map.empty;
        Registers = myRegs;
        CPSR = {N = false; Z = false; C = false; V = false }
        }
    let addInstruction (machState, count:Address) (elem:(Instr option * string option)) =
        let addIn count (elem:(Instr option * string option)) =
            match elem with
            | (Some instr, _ ) -> machState.Memory.Add(count, Instr instr)
            | _ -> failwithf "Not implemented, label error"
        let memValue = addIn count elem
        let newState = {machState with Memory = memValue}
        (newState, count + 4u)

    List.fold addInstruction (final, 0u) iList
    |> fst
    
    
let programASM = "AND R1, R2, R3, LSR #4
start ADDSNE R1, R2, R3
SUBSCC R3, R5, #34
SUBS R1, R2, #-6
loop1 MOV R1, R2
MUL R1, R2, R3
MLAS R1, R2, R3, R4
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