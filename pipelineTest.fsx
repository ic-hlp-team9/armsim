module Types

type Register = int
type Word = int
type Address = uint32
type AddressingType = Pre | Post

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
| Shift of sbyte*RegisterName


type ArithLogicInstr = {Cond: ConditionCode option; Op: ArithLogicOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FlexOp}
type MoveInstr = {Cond: ConditionCode option; Op: MoveOp; S:bool; Rd: RegisterName; Op2: FlexOp}
type TestInstr = {Cond: ConditionCode option; Op: TestOp; Rn: RegisterName; Op2: FlexOp}
type BranchInstr = {Cond: ConditionCode option; L:bool; Address: Address} //Address type TBD, 24bit field originally
type MRSInstr = {Cond: ConditionCode option; Rd:RegisterName; Psr:PSR}
type MSRInstr = {Cond: ConditionCode option; Flags: APSRFlag list ;Param: ImReg}
type ShiftInstr = {Cond: ConditionCode option; Op: ShiftOp; S:bool; Rd: RegisterName; Rn: RegisterName; Op2: FlexOp} //Last parameter is option becase RRX only has 2 registers as parameters
type MultInstr = {Cond: ConditionCode option; Op: MultOp; S:bool; Rd: RegisterName; Rm: RegisterName; Rs: RegisterName; Rn: RegisterName option} //Mul only has 3 registers as parameters that's why last one is option; MLS cannot have S suffix, therefore it is also option
type SingleMemInstr = {Cond: ConditionCode option; Op: SingleMemOp; Addressing: AddressingType; ByteAddressing: bool; Pointer: RegisterName; Rd: RegisterName; Offset: FlexOp}
type MultiMemInstr = {Cond: ConditionCode option; Op: MultMemOp; Dir: Dir; Pointer: RegisterName; Rlist: RegisterName list; WriteBack: bool}
type MemInstr =
| SingleMemInstr of SingleMemInstr
| MultiMemInstr of MultiMemInstr

type Instr =
 | ArithLogicInstr of ArithLogicInstr// Flex op2
 | MoveInstr of MoveInstr // Flex op2
 | TestInstr of TestInstr// Flex op2
 | MultInstr of MultInstr
 | ShiftInstr of ShiftInstr
 | BranchInstr of BranchInstr
 | PSRInstr of MRSInstr | MSRInstr
 | MemInstr of MemInstr


type PossiblyDecodedWord =
 | Instr of Instr
 | Word of Word

type MachineRepresentation = {
    Memory: Map<Address, PossiblyDecodedWord>;
    Registers: RegisterFile;
    CPSR: CPSR;
}
// val FrontEnd: string -> (MachineRepresentation Option * FrontendStatusItem list)




let fetch machineState =
    let PC = machineState.Registers.[R15] |> uint32
    machineState.Memory.[PC]


let secondOp flexOp machineState =
  match flexOp with
  | Const n -> int n
  | Shift (b, Rn) -> machineState.Registers.[Rn] <<< int32 b


let boolToInt = function
    | true -> 1
    | false -> 0


let writeRegister rd machineState res =
  Map.add rd res machineState.Registers


let getMemWord (byteAddressing:bool) (address:Address) (machineState:MachineRepresentation) =
  match byteAddressing with
  | false when address%4u <> 0u -> failwithf "Unaligned memory access"
  | false -> match machineState.Memory.[address] with
             | Word w -> w
             | Instr intr -> failwithf "Data access attempt within instruction space"
  | true  -> match machineState.Memory.[address-address%4u], int (address%4u) with
             | Word w, offset -> w |> (<<<)  (24-8*offset) |> (>>>) 24
             | Instr instr, _ -> failwithf "Data access attempt within instruction space"


let storeMemWord (byteAddressing:bool) (address:Address) (word:Word) (machineState:MachineRepresentation) =
 match byteAddressing with
 | false when address%4u <> 0u -> failwithf "Unaligned memory access"
 | false -> match machineState.Memory.[address] with
            | Word oldContent -> {machineState with Memory = Map.add address (Word word) machineState.Memory}
            | Instr intr -> failwithf "Data access attempt within instruction space"
 | true  -> match machineState.Memory.[address-address%4u], int (address%4u) with
            | Word w, offset -> let newWord = 0xFF |> (<<<)  (8*offset) |> (~~~) |> (&&&) w |> (|||) (word &&& 0xff <<< 8*offset)
                                {machineState with Memory = Map.add address (Word newWord) machineState.Memory}
            | Instr instr, _ -> failwithf "Data access attempt within instruction space"


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


type ArithOperation = Addition | Subtraction
type ShiftOperation = Left | Right


let getAddFlags operation carry x y =
    let a = uint64 (uint32 x)
    let b = match operation with
            | Subtraction -> uint64 (~~~(uint32 y)) + 1UL
            | Addition -> uint64 (uint32 x)
    let c = int64 x
    let d = match operation with
            | Subtraction -> - (int64 y)
            | Addition -> int64 y
    let ures = a + b + (carry |> uint64)
    let sres = c + d + (carry |> int64)
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
   res, resFlags


let getShiftFlags shiftDir f x y =
  let carryCheck =
    match shiftDir with
    | Right -> 1
    | Left -> -2147483648
  let carryRes =
    match x with
    | 0 -> y
    | _ -> f x (y-1)
  let myFlags = {N=false; Z=false; C=(carryRes &&& carryCheck <> 0); V=false}
  let res = f x y
  let resFlags = match res with
                 | 0 ->  {myFlags with Z=true}
                 | n when n < 0 -> {myFlags with N=true}
                 | _ ->  myFlags
  res, resFlags


let execArithLogicInstr (arithLogicInstr:ArithLogicInstr) (machineState:MachineRepresentation) =
    let opMatch  = function
        | AND -> (&&&), getFlags (&&&)
        | EOR -> (^^^), getFlags (^^^)
        | SUB -> (-),  getAddFlags Subtraction 0
        | RSB -> (fun a b -> b - a), fun x y -> (getAddFlags Subtraction 0) y x
        | ADD -> (+), getAddFlags Addition 0
        | ADC -> (fun a b -> (a + b + (machineState.CPSR.C |> boolToInt))), (getAddFlags Addition (machineState.CPSR.C |> boolToInt))
        | SBC -> (fun a b -> (a - b - 1 + (machineState.CPSR.C |> boolToInt))), getAddFlags Subtraction (machineState.CPSR.C |> boolToInt |> (-) 1)
        | RSC -> (fun a b -> (b - a - 1 + (machineState.CPSR.C |> boolToInt))), fun x y -> (getAddFlags Subtraction (machineState.CPSR.C |> boolToInt |> (-) 1) y x)
        | ORR -> (|||), getFlags (|||)
        | BIC -> (fun x y -> ~~~x &&& y), getFlags (fun x y -> ~~~x &&& y)
    let arithLogicFun = (opMatch (arithLogicInstr.Op) |> fst)
    let flagFun = (opMatch (arithLogicInstr.Op) |> snd)
    let op1, op2 = (machineState.Registers.[arithLogicInstr.Rn]), (secondOp arithLogicInstr.Op2 machineState)
    let res, flags =
      match arithLogicInstr.S with
      | false -> arithLogicFun op1 op2, machineState.CPSR
      | true -> flagFun op1 op2
    {machineState with Registers = writeRegister arithLogicInstr.Rd machineState res; CPSR=flags}


let execTestInstr (testInstr:TestInstr) (machineState:MachineRepresentation) =
  let opMatch = function
    | TST -> getFlags (&&&)
    | TEQ -> getFlags (^^^)
    | CMP -> getAddFlags Subtraction 0
    | CMN -> getAddFlags Addition 0
  let op1, op2 = (machineState.Registers.[testInstr.Rn]), (secondOp testInstr.Op2 machineState)
  let flags = (testInstr.Op |> opMatch) op1 op2 |> snd
  {machineState with CPSR=flags}


let execMoveInstr (movInstr:MoveInstr) (machineState:MachineRepresentation) =
    let opMatch = function
      | MOV -> id, fun x -> (x, {N = (x < 0); Z = (x=0); C=false; V=false;})
      | MVN -> (~~~), fun x -> (~~~x, {N = (~~~x < 0); Z = (~~~x=0); C=false; V=false;})
    let movFun = (opMatch (movInstr.Op) |> fst)
    let flagFun = (opMatch (movInstr.Op) |> snd)
    let op = secondOp movInstr.Op2 machineState
    let res, flags =
      match movInstr.S with
      | false -> movFun op, machineState.CPSR
      | true -> flagFun op
    {machineState with Registers=writeRegister movInstr.Rd machineState res; CPSR = flags}


let execShiftInstr (shiftInstr:ShiftInstr) (machineState:MachineRepresentation) =
    let rotateRight reg shift =
        let longReg = int64 reg
        let rotated = longReg <<< (32 - shift%32)
        int (longReg >>> shift%32 ||| rotated)

    let rorateRightX _ a = machineState.CPSR.C |> boolToInt |> fun x -> x <<< 31 |> (|||) (a >>> 1)
    let logicalShiftRight = fun a b -> int ((uint32 a) >>> b)

    let opMatch = function
      | ASR -> (>>>), getShiftFlags Right (>>>)
      | LSR -> logicalShiftRight, getShiftFlags Right logicalShiftRight
      | ROR -> rotateRight, getShiftFlags Right rotateRight
      | LSL -> (<<<), getShiftFlags Left (<<<)
      | RRX -> rorateRightX, getShiftFlags Right rorateRightX

    let shiftFun = (opMatch (shiftInstr.Op) |> fst)
    let flagFun = (opMatch (shiftInstr.Op) |> snd)
    let op1, op2 = machineState.Registers.[shiftInstr.Rn], secondOp shiftInstr.Op2 machineState

    let res, flags =
      match shiftInstr.S with
      | true -> shiftFun op1 op2, machineState.CPSR
      | false -> flagFun op1 op2
    {machineState with Registers=writeRegister shiftInstr.Rd machineState res; CPSR=flags}


let execBranchInstr (branchInstr:BranchInstr) (machineState:MachineRepresentation) =
  let dest = int branchInstr.Address
  let jumpState = {machineState with Registers = writeRegister R15 machineState dest}
  match branchInstr.L with
  | false -> jumpState
  | true ->
    let link = machineState.Registers.[R15] + 4;
    {jumpState with Registers = (writeRegister R14 machineState link)}


let execSingleMemInstr (memInstr:SingleMemInstr) (machineState:MachineRepresentation) =
  let offset = secondOp memInstr.Offset machineState
  let loadPointer, resPointer =
    match memInstr.Addressing, machineState.Registers.[memInstr.Pointer] with
    | Pre, adr -> adr, adr+offset
    | Post, adr -> adr+offset, adr+offset
  let writtenMem =
    match memInstr.Op with
    | LDR -> let memData = getMemWord memInstr.ByteAddressing (uint32 loadPointer) machineState;
             {machineState with Registers = (writeRegister memInstr.Rd machineState memData)}
    | STR -> let memData = machineState.Registers.[memInstr.Rd]
             storeMemWord memInstr.ByteAddressing (uint32 loadPointer) memData machineState
  {writtenMem with Registers = writeRegister memInstr.Pointer machineState resPointer}


let execMultiMemInstr (memInstr:MultiMemInstr) (machineState:MachineRepresentation) =
  let offset, initPointer =
    match memInstr.Dir with
    | IA -> 4, machineState.Registers.[memInstr.Pointer]
    | IB -> 4, machineState.Registers.[memInstr.Pointer] + 4
    | DA -> -4, machineState.Registers.[memInstr.Pointer]
    | DB -> -4, machineState.Registers.[memInstr.Pointer] - 4
  let memFun =
    match memInstr.Op with
    | LDM -> fun (ms, pt) reg -> ({ms with Registers = (writeRegister reg machineState (getMemWord false pt ms))}, uint32 (int pt + offset))
    | STM -> fun (ms, pt) reg -> ((storeMemWord false pt ms.Registers.[reg] ms), uint32 (int pt + offset))
  let loadedState = List.fold memFun (machineState, uint32 initPointer) memInstr.Rlist |> fst
  match memInstr.WriteBack with
  | true -> loadedState
  | false -> {loadedState with Registers = writeRegister memInstr.Pointer loadedState machineState.Registers.[memInstr.Pointer]}

let writeRegisters (registers: RegisterFile) machineState =
    let newMap = Map.fold (fun acc key value -> Map.add key value acc) machineState.Registers registers
    newMap 

let execMultInstr (multInstr:MultInstr) machineState =
    let myFlags = {N=false; Z=false; C=false; V=false}


    let umullFun rdHi (rm:Register) (rs:Register) rdLo  =
        let res : uint64 =  (uint64 (uint32 rm)) * (uint64 (uint32 rs))
        let rdHiVal  = int32 (res >>> 32)  
        let rdLoVal  = int32 res
        [rdLoVal ; rdHiVal] 

    let umlalFun rdHi (rm:Register) (rs:Register) rdLo  =
        let tmpRes : uint64 = (uint64 (uint32 rdLo )) ||| ( (uint64 (uint32 rdHi )) <<< 32 ) 
        let res : uint64 = tmpRes +  (uint64 (uint32 rm)) * (uint64 (uint32 rs))
        let rdHiVal  = int32 (res >>> 32)  
        let rdLoVal  = int32 res
        [rdLoVal ; rdHiVal]

    let smullFun rdHi (rm : Register) (rs : Register) rdLo =
        let res : int64 =  (int64  rm) * (int64  rs)
        let rdHiVal  = int32 (res >>> 32)  
        let rdLoVal  = int32 res
        [rdLoVal ; rdHiVal]

    let smlalFun rdHi (rm:Register) (rs:Register) rdLo  =
        let tmpRes : int64 = (int64  rdLo ) ||| ( (int64  rdHi ) <<< 32 ) 
        let res : int64 = tmpRes +  (int64 rm) * (int64  rs)
        let rdHiVal  = int32 (res >>> 32)  
        let rdLoVal  = int32 res
        [rdLoVal ; rdHiVal]

    let opMatch multOp =
        match multOp with
        | MUL -> (fun a b _ _ -> [a * b]) , [multInstr.Rd] 
        | MLA -> (fun a b c _ -> [c + a*b]) , [multInstr.Rd]
        | MLS -> (fun a b c _ -> [c - a*b]) , [ multInstr.Rd]
        | UMULL -> (umullFun) , [multInstr.Rd ; multInstr.Rm]
        | UMLAL -> (umlalFun) , [multInstr.Rd ; multInstr.Rm]
        | SMULL -> (smullFun) , [multInstr.Rd ; multInstr.Rm]
        | SMLAL -> (smlalFun) , [multInstr.Rd ; multInstr.Rm]
    
    let multInstrFun = (opMatch multInstr.Op)
    let rd = machineState.Registers.[multInstr.Rd]
    let rm = machineState.Registers.[multInstr.Rm]
    let rs =  machineState.Registers.[multInstr.Rs]
    let rn = match multInstr.Rn with
             | Some registerName ->  machineState.Registers.[registerName]
             | None -> 0
    
    let resValue = fst (opMatch multInstr.Op) rm rs rn rd
    let resReg = snd (opMatch multInstr.Op)
    let registers =  List.zip resReg resValue |> Map.ofList 
    {machineState with Registers = writeRegisters registers machineState }                      


let decode (possiblyInstr:PossiblyDecodedWord) =
    match possiblyInstr with
    | Word _ -> failwithf "Word decoded"
    | Instr instr ->
        match instr with
        | ArithLogicInstr instr -> fun () -> execArithLogicInstr instr
        | MoveInstr instr -> fun () -> execMoveInstr instr
        | TestInstr instr -> fun () -> execTestInstr instr
        | MultInstr instr -> fun () -> execMultInstr instr
        | ShiftInstr instr -> fun () -> execShiftInstr instr
        | BranchInstr instr -> fun () -> execBranchInstr instr
        | MemInstr instr -> match instr with
                            | SingleMemInstr instr -> fun () -> execSingleMemInstr instr
                            | MultiMemInstr instr -> fun () -> execMultiMemInstr instr
        | _ -> failwithf "not implemented"


let execute arg = fun x -> x() arg


let pipeLine machineState =
  machineState
  |> fetch
  |> decode
  |> execute {machineState with Registers= writeRegister R15 machineState (machineState.Registers.[R15] + 4)}


let rec execWrapper machineState:MachineRepresentation =
  let stoppingCondition = (machineState.Registers.[R15]/4 = List.length (Map.toList machineState.Memory))
  match stoppingCondition with
  | true -> machineState
  | false -> execWrapper (pipeLine machineState)


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
    

let programASM = "MOV R0, #1"

let res = programASM
          |> tokenise
          |> createInstList
          |> doAssembler
          |> execWrapper

res
