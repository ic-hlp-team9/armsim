module Instruction

    open types

    let fetch (machineState:MachineRepresentation) : PossiblyDecodedWord =
        let pc = machineState.Registers.[R15] |> uint32
        match Map.containsKey (pc-8u) machineState.Memory  with
        | false -> failwithf "Runtime Error: Empty memory cell accessed during instruction fetch"
        | true -> machineState.Memory.[pc-8u]


    let boolToInt = function
        | true -> 1
        | false -> 0


    let writeRegister (rd:RegisterName) (machineState:MachineRepresentation) (res:int) : RegisterFile = //Returns a new immutable Register File having performed a single write operation
      Map.add rd res machineState.Registers


    let incrementPc (machineState:MachineRepresentation) : MachineRepresentation =
      {machineState with Registers = writeRegister R15 machineState (machineState.Registers.[R15] + 4)}


    let barrelShift (op:ShiftOp) (data:Register) (shift:ImReg) (machineState:MachineRepresentation) : int*bool = //Returns the result of a shift operation in a tuple with the produced carry
      let getCarry (shiftDir:ShiftOperation) (f:int->int->int) (shiftVal:int) (data:int) : int*bool =
        let carryCheck =
          match shiftDir with
          | Right -> 1
          | Left -> -2147483648
        match shiftVal with
          | 0 -> data, false
          | _ -> let carryRes = f (data) (shiftVal-1);
                 (f data shiftVal), (carryRes &&& carryCheck <> 0)

      let ror (reg:int) (shift:int) =
          let longReg = int64 reg
          let rotated = longReg <<< (32 - shift%32)
          int (longReg >>> shift%32 ||| rotated)

      let rrx (a:int) _ = machineState.CPSR.C |> boolToInt |> fun x -> x <<< 31 |> (|||) (a >>> 1)
      let lsR = fun (a:int) (b:int) -> int ((uint32 a) >>> b)

      let shiftVal =
        match shift with
        | Immediate n when n > 32 || n < 0 -> failwithf "Invalid shift immediate value"
        | Immediate n -> n
        | Register r -> machineState.Registers.[r] &&& 255

      let shiftFun =
        match op with
        | ASR -> getCarry Right (>>>)
        | LSR -> getCarry Right lsR
        | ROR -> getCarry Right ror
        | LSL -> getCarry Left (<<<)
        | RRX -> getCarry Right rrx

      shiftFun shiftVal data

    let secondOp (flexOp:FlexOp) (machineState:MachineRepresentation) : int*bool =
      match flexOp with
      | Const n -> int n, false
      | Shift (sOp, shift, Rn) -> barrelShift sOp machineState.Registers.[Rn] shift machineState


    let getMemWord (byteAddressing:bool) (address:Address) (machineState:MachineRepresentation) : Word =
      match Map.containsKey (address-address%4u) (machineState.Memory) with
      | false -> 0
      | true ->  match byteAddressing with
                 | false when address%4u <> 0u -> failwithf "Runtime Error: Unaligned memory access"
                 | false -> match machineState.Memory.[address] with
                            | Word w -> w
                            | Instr intr -> failwithf "Runtime Error: Data access attemp within instruction space"
                 | true  -> match machineState.Memory.[address-address%4u], int (address%4u) with
                            | Word w, offset -> w |> (<<<)  (24-8*offset) |> (>>>) 24
                            | Instr instr, _ -> failwithf "Runtime Error: Data access attemp within instruction space"


    let storeMemWord (byteAddressing:bool) (address:Address) (word:Word) (machineState:MachineRepresentation) : MachineRepresentation =
     match byteAddressing with
     | false when address%4u <> 0u -> failwithf "Runtime Error: Unaligned memory access"
     | false when not (Map.containsKey (address-address%4u) (machineState.Memory)) -> {machineState with Memory = Map.add address (Word word) machineState.Memory}
     | false -> match machineState.Memory.[address] with
                | Word oldContent -> {machineState with Memory = Map.add address (Word word) machineState.Memory}
                | Instr intr -> failwithf "Runtime Error: Data access attemp within instruction space"
     | true  -> match machineState.Memory.[address-address%4u], int (address%4u) with
                | Word w, offset -> let newWord = 0xFF |> (<<<)  (8*offset) |> (~~~) |> (&&&) w |> (|||) (word &&& 0xff <<< 8*offset)
                                    {machineState with Memory = Map.add address (Word newWord) machineState.Memory}
                | Instr instr, _ -> failwithf "Runtime Error: Data access attemp within instruction space"


    let getAddFlags (operation:ArithOperation) (carry:int) (x:int) (y:int) : int*CPSR = //Returns a tuple of result and all flags computed on ArithOperation (x y) with optional carry/borrow in
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


    let getFlags (f: int -> int -> int) (x:int) (y:int) : int*CPSR = //Returns a tuple of result and N, Z flags computed on f (x y)
       let res = f x y
       let myFlags = {N=false; Z=false; C=false; V=false}
       let resFlags = match res with
                      | 0 ->  {myFlags with Z=true}
                      | n when n < 0 -> {myFlags with N=true}
                      | _ ->  myFlags
       res, resFlags


    let execArithLogicInstr (arithLogicInstr:ArithLogicInstr) (machineState:MachineRepresentation) : MachineRepresentation =
        let opMatch  = function
            | AND -> (&&&), getFlags (&&&)
            | EOR -> (^^^), getFlags (^^^)
            | SUB -> (-),  getAddFlags Subtraction 0
            | RSB -> (fun a b -> b - a), fun x y -> (getAddFlags Subtraction 0) y x
            | ADD -> (+), getAddFlags Addition 0
            | ADC -> (fun a b -> (a + b + (machineState.CPSR.C |> boolToInt))), (getAddFlags Addition (machineState.CPSR.C |> boolToInt))
            | SBC -> (fun a b -> (a - b - 1 + (machineState.CPSR.C |> boolToInt))), getAddFlags Subtraction (-1 + (machineState.CPSR.C |> boolToInt))
            | RSC -> (fun a b -> (b - a - 1 + (machineState.CPSR.C |> boolToInt))), fun x y -> (getAddFlags Subtraction (-1 + (machineState.CPSR.C |> boolToInt)) y x)
            | ORR -> (|||), getFlags (|||)
            | BIC -> (fun x y -> x &&& ~~~y), getFlags (fun x y -> x &&& ~~~y)
        let arithLogicFun = (opMatch (arithLogicInstr.Op) |> fst)
        let flagFun = (opMatch (arithLogicInstr.Op) |> snd)
        let op1 = (machineState.Registers.[arithLogicInstr.Rn])
        let op2, carry = (secondOp arithLogicInstr.Op2 machineState)
        let res, flags =
          match arithLogicInstr.S with
          | false -> arithLogicFun op1 op2, machineState.CPSR
          | true -> let r, f = flagFun op1 op2;
                    match arithLogicInstr.Op with
                    | AND | EOR | ORR | BIC -> r, {f with C = carry}
                    | _ -> r, f
        {machineState with Registers = writeRegister arithLogicInstr.Rd machineState res; CPSR=flags}


    let execTestInstr (testInstr:TestInstr) (machineState:MachineRepresentation) : MachineRepresentation =
      let opMatch = function
        | TST -> getFlags (&&&)
        | TEQ -> getFlags (^^^)
        | CMP -> getAddFlags Subtraction 0
        | CMN -> getAddFlags Addition 0
      let op1 = machineState.Registers.[testInstr.Rn]
      let op2, carry = secondOp testInstr.Op2 machineState
      let flags =
        let resFlags = (testInstr.Op |> opMatch) op1 op2 |> snd;
        match testInstr.Op with
        | CMN ->  resFlags
        | _  -> {resFlags with C=carry}
      {machineState with CPSR=flags}


    let execMoveInstr (movInstr:MoveInstr) (machineState:MachineRepresentation) : MachineRepresentation =
        let opMatch = function
          | MOV -> id, fun x -> (x, {N = (x < 0); Z = (x=0); C=false; V=false;})
          | MVN -> (~~~), fun x -> (~~~x, {N = (~~~x < 0); Z = (~~~x=0); C=false; V=false;})
        let movFun = (opMatch (movInstr.Op) |> fst)
        let flagFun = (opMatch (movInstr.Op) |> snd)
        let op, carry = secondOp movInstr.Op2 machineState
        let res, flags =
          match movInstr.S with
          | false -> movFun op, machineState.CPSR
          | true -> flagFun op |> fun (r, f) -> r, {f with C=carry}

        {machineState with Registers=writeRegister movInstr.Rd machineState res; CPSR = flags}
        |> fun ms -> if (movInstr.Rd=R15) then (incrementPc ms) else ms



    let execShiftInstr (shiftInstr:ShiftInstr) (machineState:MachineRepresentation) : MachineRepresentation =
        let op1, op2 = machineState.Registers.[shiftInstr.Rn], (shiftInstr.Op2)
        let res, carry = barrelShift shiftInstr.Op op1 op2 machineState
        let flags =
          match shiftInstr.S with
          | true ->  {N = (res < 0); Z = (res=0); C=carry; V=false;}
          | false -> machineState.CPSR
        {machineState with Registers=writeRegister shiftInstr.Rd machineState res; CPSR=flags}


    let execBranchInstr (branchInstr:BranchInstr) (machineState:MachineRepresentation) : MachineRepresentation =
      let dest = int branchInstr.Address + 4
      let jumpState = {machineState with Registers = writeRegister R15 machineState dest}
      match branchInstr.L with
      | false -> jumpState
      | true ->
        let link = machineState.Registers.[R15] - 4;
        {jumpState with Registers = (writeRegister R14 jumpState link)}


    let execSingleMemInstr (memInstr:SingleMemInstr) (machineState:MachineRepresentation) : MachineRepresentation =
      let offset = secondOp memInstr.Offset machineState |> fst
      let loadPointer, resPointer =
        match memInstr.Addressing, machineState.Registers.[memInstr.Pointer] with
        | Offset, adr -> adr+offset, adr
        | Pre, adr -> adr+offset, adr+offset
        | Post, adr -> adr, adr+offset
      let writtenMem =
        match memInstr.Op with
        | LDR -> let memData = getMemWord memInstr.ByteAddressing (uint32 loadPointer) machineState;
                 {machineState with Registers = (writeRegister memInstr.Rd machineState memData)}
        | STR -> let memData = machineState.Registers.[memInstr.Rd]
                 storeMemWord memInstr.ByteAddressing (uint32 loadPointer) memData machineState
      {writtenMem with Registers = (writeRegister memInstr.Pointer writtenMem resPointer)}


    let execMultiMemInstr (memInstr:MultiMemInstr) (machineState:MachineRepresentation) : MachineRepresentation =
      let pointer = machineState.Registers.[memInstr.Pointer]
      let offset, initPointer, sortedRegList =
        match memInstr.Dir with
        | IA -> 4, pointer, List.sort (memInstr.Rlist)
        | IB -> 4, pointer + 4, List.sort (memInstr.Rlist)
        | DA -> -4, pointer, List.sort (memInstr.Rlist) |> List.rev
        | DB -> -4, pointer - 4, List.sort (memInstr.Rlist) |> List.rev
      let memFun =
        match memInstr.Op with
        | LDM -> fun (ms, pt) reg -> ({ms with Registers = (writeRegister reg ms (getMemWord false pt ms))}, uint32 (int pt + offset))
        | STM -> fun (ms, pt) reg -> ((storeMemWord false pt ms.Registers.[reg] ms), uint32 (int pt + offset))
      let loadedState, finalPointer = List.fold memFun (machineState, uint32 initPointer) sortedRegList |> fst, pointer + offset*(List.length sortedRegList)
      match memInstr.WriteBack with
      | true -> {loadedState with Registers = writeRegister memInstr.Pointer loadedState (int finalPointer)}
      | false -> {loadedState with Registers = writeRegister memInstr.Pointer loadedState machineState.Registers.[memInstr.Pointer]}
      |> fun ms -> if (List.contains R15 memInstr.Rlist) then (incrementPc ms) else ms

    let writeRegisters (registers: RegisterFile) (machineState:MachineRepresentation) : RegisterFile =
      Map.fold (fun acc key value -> Map.add key value acc) machineState.Registers registers


    let execMultInstr (multInstr:MultInstr) (machineState:MachineRepresentation) : MachineRepresentation =
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
        {machineState with Registers = writeRegisters registers machineState}


    let condMatch (machineState:MachineRepresentation) (cond:ConditionCode option) : bool =
      let n, z = machineState.CPSR.N, machineState.CPSR.Z
      let c, v =  machineState.CPSR.C, machineState.CPSR.V
      match cond with
      | Some EQ -> z
      | Some NE -> not z
      | Some CS -> c
      | Some CC -> not c
      | Some MI -> n
      | Some PL -> not n
      | Some VS -> v
      | Some VC -> not v
      | Some HI -> c && not z
      | Some LS -> not c && z
      | Some GE -> n=v
      | Some LT -> n<>v
      | Some GT -> not z && n=v
      | Some LE -> z || n<>v
      | Some AL | None -> true
      | Some NV -> false


    let decode (possiblyInstr:PossiblyDecodedWord) (machineState:MachineRepresentation) : (MachineRepresentation->MachineRepresentation) =
        match possiblyInstr with
        | Word _ -> failwithf "Runtime Error: Data word encountered during instruction decode"
        | Instr instr ->
            //printfn "%A" instr;
            match instr with
            | cond, someInstr when not (condMatch machineState cond) -> execMoveInstr {Op=MOV; S=false; Rd=R0; Op2=Shift (LSL, Immediate 0, R0)}
            | _ -> match snd instr with
                   | ArithLogicInstr instr -> execArithLogicInstr instr
                   | MoveInstr instr -> execMoveInstr instr
                   | TestInstr instr -> execTestInstr instr
                   | MultInstr instr -> execMultInstr instr
                   | ShiftInstr instr -> execShiftInstr instr
                   | BranchInstr instr -> execBranchInstr instr
                   | MemInstr memInstr -> match memInstr with
                                          | SingleMemInstr instr -> execSingleMemInstr instr
                                          | MultiMemInstr instr -> execMultiMemInstr instr
                   | _ -> failwithf "Runtime Error: Instruction not implemented"


    let execute ms = fun x -> x ms ms


    let pipeLine (machineState:MachineRepresentation) : MachineRepresentation =
      let myMs =
          machineState
          |> fetch
          |> decode
          |> execute machineState
      {myMs with Registers=writeRegister R15 myMs (myMs.Registers.[R15]+4)}


    let rec execWrapper (machineState:MachineRepresentation) : MachineRepresentation =
      let filterInstr = function
      | Instr instr -> true
      | Word word -> false
      let lastAddress = 4*(machineState.Memory |> Map.toList |> List.map snd |> List.filter (filterInstr) |> List.length)
      let stoppingCondition = (machineState.Registers.[R15]-8 = lastAddress)
      match stoppingCondition with
      | true -> machineState
      | false -> execWrapper (pipeLine machineState)

    let rec execFinite n ms =
        //printfn "%A" (Map.toList (ms.Memory));
        printf "%A ;" (Map.toList (ms.Registers));
        //printf "%A" ms.CPSR;
        match n with
        | 0 -> ms
        | _ -> execFinite (n-1) (pipeLine ms)
