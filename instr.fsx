#load "types.fsx"
open Types



let fetch machineState =
    let PC = machineState.Registers.[R15]
    machineState.Memory.[int (PC)/4]

let decode (possiblyInstr:PossiblyDecodedWord) =
    match possiblyInstr with
    | Word _ -> failwithf "Word decoded"
    | Instr instr ->  
        match instr with
        | ArithLogicInstr _ -> fun () -> (execArithLogicInstr instr machineState)
        | MoveInstr 
        | TestInstr
        | MultInstr
        | ShiftInstr
        | BranchInstr
        | PSRInstr
        | MemInstr
        | MiscInstr
    

let secondOp flexOp =
    match flexOp with
    | Const n -> n
    | Shift b, Rn -> machineState.registers[Rn] << b


let execMoveInstr movInstr machineState =
    let f =
        match movInstr.Op with
        | MOV -> id
        | MVN (~~~)


let rec execArithLogicInstr arithLogicInstr machineState =
    let opMatch aluOp =
        match aluOp with
        | AND -> (&&&)
        | EOR -> (^^^)
        | SUB -> (+)
        | RSB -> fun a b -> b-a
        | ADD -> fun a b -> a+b
        | ADC -> (+)
        | SBC -> (-)
        | RSC -> fun a b -> b-a
        | ORR -> (|||)
        | BIC -> (~~~)

    let flags =
      match arithLogicInstr.S with
      | false -> machineState.flags
      | true -> generateFlags

    {machineState with arithLogicInstr.Rd=(opMatch ArithLogicOp)}

let pipeLine machineState =
  machineState
  |> fetch
  |> decode
  |> execute

let rec execWrapper machineState:MachineRepresentation =
  match stopCondition with
  | true -> machineState
  | false -> execWrapper (pipeLine machineState)

let execute = fun x -> x()

