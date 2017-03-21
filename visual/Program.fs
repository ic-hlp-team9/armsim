namespace VisualInterface

module Program=

    open Expecto
    open types
    open parser
    open Instruction
    open VisualInterface
    open Microsoft.FSharp.Reflection
    open FsCheck
    open test
    open System.IO

    /// postlude which sets R1 bits to status bit values
    let NZCVToR12 =
       """
          MOV R1, #0
          ADDMI R1, R1, #8
          ADDEQ R1, R1, #4
          ADDCS R1, R1, #2
          ADDVS R1, R1, #1
       """ 

    let defaultParas = {
            Cached = false                  // true if results are stored in a cache on disk and reused to speed 
                                            // up future repeat simulations
            VisualPath =  @"..\..\..\visualapp\visual\" // the directory in which the downloaded VisUAL.exe can be found
            WorkFileDir = @"..\..\..\VisualWork\"      // the directory in which both temporary files and the persistent cache file are put
            MemDataStart = 0x100            // start of VisUAL data section Memory
            MemLocs = []                    // memory locations to be traced and data returned

        }

    type Flags = 
        {
            FN: bool
            FZ: bool
            FC: bool
            FV: bool
        }
   
    let defParasWithLocs locs = {defaultParas with MemLocs = locs}
    
    /// Adds postlude to assembly code to detect flags values.
    /// Returns registers (before flag detection code) * flags
    let RunVisualWithFlagsOut paras src =
        let asm = src + NZCVToR12
        let trace = VisualInterface.RunVisual defaultParas asm
        if Array.length trace < 5 then failwithf "Error: Trace \n%A\nfrom\n%s\n has length %d < 5." trace asm (Array.length trace)
        let regs = 
            [0..15] 
            |> List.map (fun n -> R n, trace.[5].ResOut.[R n]) // get reg values before postlude
            |> Map.ofList
        let flagsInt = trace.[0].ResOut.[R 1] //Postlude code sets R1(3:0) equal to NZCV
        printfn "flagsint=%x, trace=%A" flagsInt trace.[5]
        let flagBool n = (flagsInt &&& (1 <<< n)) > 0
        { 
          FN = flagBool 3
          FZ = flagBool 2
          FC = flagBool 1
          FV = flagBool 0
        }, regs

    /// Run Visual with specified source code and list of memory locations to trace
    /// src - source code
    /// memLocs - list of memory locations to trace
    let RunVisualWithFlagsOutLocs memLocs src =
        RunVisualWithFlagsOut {defaultParas with MemLocs = memLocs} src

    /// convenience function, convert 4 char string to NZCV status flag record
    let strToFlags s =
        let toBool = function | '0' -> false | '1' -> true | s -> failwithf "Bad character in flag specification '%c'" s
        match s |> Seq.toList |> List.map toBool with
        | [ a ; b ; c ; d] -> { FN=a; FZ=b;FC=c;FV=d}
        | _ -> failwithf "Wrong number of characters (should be 4) in flag specification %s" s
    
    
    /// run an expecto test of VisUAL
    /// name - name of test
    ///
    let VisualUnitTest name src (flagsExpected:string) (outExpected: (Out * int) list) =
        testCase name <| fun () ->
            let mems = outExpected |> List.collect (function | Mem n, x -> [n,x] | _ -> [])
            let memLocs = mems |> List.map fst
            let flags, outs = RunVisualWithFlagsOutLocs memLocs src
            Expecto.Expect.equal flags (flagsExpected |> strToFlags)  ( sprintf "Status flags don't match with instructions: \n%s" src)
            let regs = outExpected |> List.filter (function | R _,_ -> true | _ -> false)   
            let getOut (out, v) = 
                try
                    out, outs.[out]
                with
                | _ -> failwithf "Can't find output %A in outs %A" out outs
            Expecto.Expect.sequenceEqual  outExpected (outExpected |> List.map getOut) (sprintf "Reg and Mem outputs don't match with instruction: \n%s\n" src)  
    
          
    let seqConfig = { Expecto.Tests.defaultConfig with parallel = false}

    let wrapper (src:string) : MachineRepresentation =
        src
        |> tokenise
        |> createInstList
        |> doAssembler
        |> execWrapper
        

    let unwrapMachRep (outMachRep:MachineRepresentation) :((Out * int) list) = 
        //let transformMap mIn mOut  = Map.fold (fun key value -> match key value with | k Register v ->  Map.add (R key) (int value)) mOut mIn 
        let listForTest = Map.toList  outMachRep.Registers |> List.indexed
        let filterList = List.filter (fun (i,_) -> i < 15) listForTest //don't include program counter
        let convertList = List.map  (fun (i,(_,v)) -> ((R i), int v)) filterList 
        convertList

    let unwrapFlags (outMachRep:MachineRepresentation) : string =
        let matchFlag x =
            match x with
            | false -> sprintf "0"
            | true -> sprintf "1"
        let listOfFlags m: string list = [matchFlag m.CPSR.N ; matchFlag m.CPSR.Z ; matchFlag m.CPSR.C ; matchFlag m.CPSR.V ]
        String.concat "" (listOfFlags outMachRep)
    
    

    let testWrapper (nameOfTest:string) (nLines:int) : Test =
        let src = myRandInstr |> Gen.sample 256 nLines |> cmdString
        let myMachRep = 
            try 
                src |> wrapper 
            with
                | Failure msg -> printfn "ERROR!!!!!!!!!!!!!!  %A" msg ; {
                                                    Memory = Map.empty;
                                                    Registers = myRegs;
                                                    CPSR = {N = false; Z = false; C = false; V = false }
                                                    DataPointer = 0x100u; 
                                                }
        
        VisualUnitTest nameOfTest src (unwrapFlags myMachRep) (myMachRep |> unwrapMachRep)


    [<EntryPoint>]
    let main args = 
        InitCache defaultParas.WorkFileDir // read the currently cached info from disk to speed things up
        let tests = 
            testList "Visual tests" [
                //testWrapper "random test1" 10
                //testWrapper "random test2" 10
                for i in [1..20] do yield testWrapper "random test1" 1
            ]
        let rc = runTests seqConfig tests
        System.Console.ReadKey() |> ignore                
        rc // return an integer exit code - 0 if all tests pass

        
       
    