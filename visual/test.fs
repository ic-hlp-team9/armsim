module test
    open System
    open testTypes
    open FsCheck
    open Microsoft.FSharp.Reflection
    
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
    let flexOp2GenCondShift (instr:ShiftInstr) : bool =
        match instr.Op2 with
        | Immediate n when n < 0 || n > 32 -> false
        | _ -> true 

    let instrType (instrType:InstrType) :bool =
        match instrType with
        |ArithLogicInstr instr -> flexOp2GenCondArt instr
        | MoveInstr instr ->  flexOp2GenCondMov instr
        | TestInstr instr -> flexOp2GenCondTst instr
        | MultInstr instr -> false //not implemented
        | ShiftInstr instr -> flexOp2GenCondShift instr
        | BranchInstr instr -> false //not implemented
        | PreAssembleBI instr -> false //not implemented
        | PreAssembleAL instr -> false //notimplemented
        | MemInstr instr -> false //not implemented


    let myRandInstr = Arb.generate<InstrType> |> Gen.filter instrType

    let typeToString (x:'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name
        | _ -> failwithf "problem with typeToString function"

    let getOp2 (op2:FlexOp) : string=
        let matchShift x =
            match x with
            | Immediate n -> sprintf "#%d" (int n)
            | Register r -> typeToString r
    
        match op2 with
        | Const n ->   sprintf "#%d" (int n)
        | Shift (sOp, shift, Rn) when sOp = RRX -> [typeToString Rn ; "," ; typeToString sOp ] |> String.concat " "
        | Shift (sOp, shift, Rn) -> [ typeToString Rn ; "," ; typeToString sOp ; matchShift shift ] |> String.concat " "

    let getOp2ForShift (op2:ImReg) : string =
        match op2 with
        |Immediate n -> sprintf "#%d" (int n)
        |Register r -> typeToString r
                                                      
    //initialize memory
    let randRegs :string = Gen.choose(-214748364,2147483647) |> Gen.sample 0 10 |> List.map (fun n -> string n) |> String.concat ", "
    //let memCmd = String.concat "\n" [(String.concat "" ["dta   DCD  "; randRegs]);"LDR	R0, =dta" ; "LDMFD	R0!, {R1, R2, R3, R4, R5,R6,R7,R8,R9}"]
    let memCmd = String.concat "\n" ["dta   DCD    10, 50, 154252, 53, 0, 1, 15, 1000, 213, 12";"LDR	R0, =dta" ; "LDMFD	R0!, {R1, R2, R3, R4, R5,R6,R7,R8,R9}"]

    //let convertArttypeToString instr :string =
    let makeStringsArthInstr (instr:ArithLogicInstr) : string list  = 
        [typeToString instr.Op;
         (if instr.S = true then "S" else "")  ;
         (if instr.CondCode = None then "" else typeToString instr.CondCode); 
         " " ;
         typeToString instr.Rd; "," ;
         typeToString instr.Rn; "," ; 
         getOp2 instr.Op2 ] 

    let makeStringsMoveInstr (instr:MoveInstr) : string list =
         [typeToString instr.Op;
         (if instr.S = true then "S" else "")  ;
         (if instr.CondCode = None then "" else typeToString instr.CondCode); 
         " " ;
         typeToString instr.Rd; "," ;
         getOp2 instr.Op2 ]

    let makeStringstestInstr (instr:TestInstr) : string list =
         [typeToString instr.Op;
         (if instr.CondCode = None then "" else typeToString instr.CondCode);
         " " ;
         typeToString instr.Rn; "," ;
         getOp2 instr.Op2 ]
    let makeStringsShiftInstr (instr:ShiftInstr) : string list =
         [typeToString instr.Op;
         (if instr.S = true then "S" else "")  ;
         (if instr.CondCode = None then "" else typeToString instr.CondCode);
         " " ;
         typeToString instr.Rd; "," ;
         (if instr.Op = RRX then "" else typeToString instr.Rn); (if instr.Op = RRX then "" else ",") ;
         getOp2ForShift instr.Op2 ]


    let matchForMake (ins: InstrType): string list =
        match ins with
        |ArithLogicInstr ins -> makeStringsArthInstr ins
        | MoveInstr ins -> makeStringsMoveInstr ins
        | TestInstr ins -> makeStringstestInstr ins
        //| MultInstr ins -> false //not implemented
        | ShiftInstr ins -> makeStringsShiftInstr ins
        //| BranchInstr ins -> false //not implemented
        //| MemInstr ins -> false //not implemented
          
    let cmd randInp = (List.map (fun x -> String.concat "" (matchForMake x)) randInp) |> String.concat "\n"
    let cmdString cmdList  :string = String.concat "\n" [memCmd ; (cmd cmdList)]  


    //let a = Gen.choose(-2147483648,2147483647) |> Gen.sample 0 10 |> List.map (fun n -> string n) |> String.concat ", "

