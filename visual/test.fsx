System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)


#load "types.fs"
#load "testTypes.fs"

#I @"packages\FsCheck.2.6.2\lib\net45"  
#r @"FsCheck.dll"

open System
open testTypes
open FsCheck
open Microsoft.FSharp.Reflection
open System.IO

let immBiggerThan0 (instr:ArithLogicInstr) : bool =
    match instr.Op2 with
    | Const n when n < 0 -> false
    | Shift (_,Immediate n,_) when n < 0 -> false
    | _ -> true 


let myRandRegs = Arb.generate<ArithLogicInstr> |> Gen.filter immBiggerThan0  
let randInstr : ArithLogicInstr list = 
    myRandRegs
    |> Gen.sample 100 10

let toString (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name
    | _ -> failwithf "problem with toString function"

let getOp2 (op2:FlexOp) : string=
    let matchShift x =
        match x with
        | Immediate n -> sprintf "#%d" (int n)
        | Register r -> toString r
    
    match op2 with
    | Const n ->   sprintf "#%d" (int n)
    | Shift (sOp, shift, Rn) -> [ toString Rn ; "," ; toString sOp ; matchShift shift ] |> String.concat " "
                                                      
    
let makeStringsArthInstr (instr:ArithLogicInstr) : string list  = 
    [toString instr.Op;
        (if instr.S = true then "S" else "")  ; " " ;
        toString instr.Rd; "," ;
        toString instr.Rn; "," ; 
        getOp2 instr.Op2 ] 
let cmd= (List.map (fun x -> String.concat "" (makeStringsArthInstr x)) randInstr) |> String.concat "\n"  
//let cmd = String.concat "" (makeStringsArthInstr randInstr.Head)

let flexOp2GenCondArt (instr:ArithLogicInstr) : bool =
    match instr.Op2 with
    | Const n when n < 0 -> false
    | Shift (_,Immediate n,_) when n < 0 || n>32 -> false
    | _ -> true
let flexOp2GenCondMov (instr:MoveInstr) : bool =
    match instr.Op2 with
    | Const n when n < 0 -> false
    | Shift (_,Immediate n,_) when n < 0 || n>32 -> false
    | _ -> true 
let flexOp2GenCondTst (instr:TestInstr) : bool =
    match instr.Op2 with
    | Const n when n < 0 -> false
    | Shift (_,Immediate n,_) when n < 0 || n>32 -> false
    | _ -> true 

let instrType (instrType:InstrType) :bool =
    match instrType with
    |ArithLogicInstr instr -> false//flexOp2GenCondArt instr
    | MoveInstr instr -> false//flexOp2GenCondMov instr
    | TestInstr instr -> false//flexOp2GenCondTst instr
    | MultInstr instr -> false //not implemented
    | ShiftInstr instr -> false //not implemented
    | BranchInstr instr -> false //not implemented
    | PreAssembleBI instr -> false //not implemented
    | PreAssembleAL instr -> false //notimplemented
    | MemInstr instr -> true //not implemented


let myRandInstr = Arb.generate<InstrType> |> Gen.filter instrType
Gen.sample 10 256 myRandInstr