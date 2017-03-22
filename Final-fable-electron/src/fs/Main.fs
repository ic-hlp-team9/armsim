module App.Main
open Fable.Import
open App.CodeMirrorInterface
open App.Renderer
open types
open parser
open instruction

let toHex (n:uint32) =
        let mapToString = function 
        | 10u -> "A"
        | 11u -> "B"
        | 12u -> "C"
        | 13u -> "D"
        | 14u -> "E"
        | 15u -> "F"
        | x -> string(x)
        List.fold (fun (myNum, myStr) thisShift -> (myNum, mapToString ((myNum >>> thisShift) &&& 15u) + myStr) ) ((uint32 n), "") [0..4..28] |> snd


let renderRegs machState =

    let lisRegs = Map.toList machState.Registers
    let bulgarianSequence = [0;1;8;9;10;11;12;13;14;15;2;3;4;5;6;7]
    //for y in [0..14] do
        //printfn "%d" (snd(lisRegs.[bulgarianSequence.[y]]))
    //printfn "%A" machState

    "<table class='ui definition table'>"
    + "<thead>"
    + "<tr><th></th>"
    + "<th>Decimal</th>"
    + "<th>Hex</th>"
    + "</tr></thead>"
    + "<tbody>"
    + String.concat "" [
        for y in [0..15] do
            yield "<tr><td>R" + sprintf "%A" y + "</td><td>" + sprintf "%d" (snd(lisRegs.[bulgarianSequence.[y]])) + "</td><td>" + sprintf "0x%s" (toHex (uint32 (snd(lisRegs.[bulgarianSequence.[y]]))))  + "</td></tr>" ]
    + "</tbody></table>"

let renderCPSR machState = 
    "<table class='ui single line table'>"
    + "<thead>"
    + "<tr>"
    + "<th>N (Negative)</th>"
    + "<th>Z (Zero)</th>"
    + "<th>C (Carry)</th>"
    + "<th>V (Overflow)</th>"
    + "</tr></thead>"
    + "<tbody>"
    + "<tr><td>" + sprintf "%A" machState.CPSR.N + "</td><td>" + sprintf "%A" machState.CPSR.Z + "</td><td>" + sprintf "%A" machState.CPSR.C + "</td><td>" + sprintf "%A" machState.CPSR.V + "</td></tr>" 
    + "</tbody></table>"


let renderMem machState = 
    let findValue memMap key =
        if Map.containsKey key memMap then
            match (Map.tryFind key memMap) with
            | Some (Word x) -> Some x
            | _ -> None
        else
            None
    
    if Map.containsKey machState.DataPointer machState.Memory then
        "<table class='ui definition table'>"
        + "<thead>"
        + "<tr><th></th>"
        + "<th>Word value (dec)</th>"
        + "<th>Word value (hex)</th>"
        + "</tr></thead>"
        + "<tbody>"
        + String.concat "" [
            for y in [0..11] do
                let contentMem = (findValue machState.Memory (machState.DataPointer + uint32(y*4)));
   
                match contentMem with
                | Some x -> yield "<tr><td>0x" + sprintf "%s" (toHex (machState.DataPointer + uint32(y*4)))  + "</td><td>" + sprintf "%d" x + "</td><td>0x" + sprintf "%s" (toHex (uint32 x)) + "</td></tr>" 
                | None -> yield ""
                ]
        + "</tbody></table>"
    else
        "<p style='padding-top: 15px; text-align: center; size: 20px; padding-bottom: 15px;'> <b>No data in memory</b> </p>"


let renderInitial() =
    let ourDiv = getById<Fable.Import.Browser.HTMLDivElement> "regs"
    let statusDiv = getById<Fable.Import.Browser.HTMLDivElement> "status"
    let memDiv = getById<Fable.Import.Browser.HTMLDivElement> "memory"
    let errDiv = getById<Fable.Import.Browser.HTMLDivElement> "msg"
    errDiv.innerHTML <- ""
    let initialState = 
        "<table class='ui definition table'>"
        + "<thead>"
        + "<tr><th></th>"
        + "<th>Decimal</th>"
        + "<th>Hex</th>"
        + "</tr></thead>"
        + "<tbody>"
        + String.concat "" [
            for y in [0..15] do
                yield "<tr><td>R" + sprintf "%A" y + "</td><td>" + "0" + "</td><td>" + "0x00000000" + "</td></tr>" ]
        + "</tbody></table>"
    ourDiv.innerHTML <- initialState
    let initialState1 =
        "<table class='ui single line table'>"
        + "<thead>"
        + "<tr>"
        + "<th>N (Negative)</th>"
        + "<th>Z (Zero)</th>"
        + "<th>C (Carry)</th>"
        + "<th>V (Overflow)</th>"
        + "</tr></thead>"
        + "<tbody>"
        + "<tr><td>" + "false" + "</td><td>" + "false" + "</td><td>" + "false" + "</td><td>" + "false" + "</td></tr>" 
        + "</tbody></table>"
    statusDiv.innerHTML <- initialState1
    let initialState2 =
        "<p style='padding-top: 15px; text-align: center; size: 20px; padding-bottom: 15px;'> <b>Nothing to show for memory content</b> </p>"
    memDiv.innerHTML <- initialState2

let renderError msg =
    let errDiv = getById<Fable.Import.Browser.HTMLDivElement> "msg"
    errDiv.innerHTML <- "<div style='border: 1px solid; color: red'>"
        + "<p style='padding-top: 15px; padding-right: 10px;"
        + "padding-left: 10px; color: black; padding-bottom: 15px;'>" + sprintf "%s" msg
        + "</p></div>"


let renderWindow tableStr1 tableStr2 tableStr3 =
    let ourDiv = getById<Fable.Import.Browser.HTMLDivElement> "regs"
    ourDiv.innerHTML <- tableStr1
    let statusDiv = getById<Fable.Import.Browser.HTMLDivElement> "status"
    statusDiv.innerHTML <- tableStr2
    let memoryDiv = getById<Fable.Import.Browser.HTMLDivElement> "memory"
    memoryDiv.innerHTML <- tableStr3
    let errDiv = getById<Fable.Import.Browser.HTMLDivElement> "msg"
    errDiv.innerHTML <- ""

   
     


let main () =
    printfn "Starting..."
    let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
    printfn "Creating editor"
    let cmEditor = App.CodeMirrorImports.CodeMirror.fromTextArea(editId, initOptions)
    printfn "Setting editor value"
    cmEditor.setValue ""
    //printfn "Line tokens: %A" (cmEditor.getLineTokens 0)
    renderInitial()
    let compileOnClick() =
        (*
        let res  =  
            cmEditor.getValue()
            |> tokenise
            |> createInstList
            |> doAssembler
            |> execWrapper
        renderWindow (renderRegs res) (renderCPSR res) (renderMem res)
        *)
        try
            let res  =  
                cmEditor.getValue()
                |> tokenise
                |> createInstList
                |> doAssembler      
                |> execWrapper
            renderWindow (renderRegs res) (renderCPSR res) (renderMem res)
        with
            | :? System.Exception as ex -> renderError ex.Message

    let compile = getById<Fable.Import.Browser.HTMLButtonElement> "compile" 
    compile.addEventListener_click(fun _ -> compileOnClick(); null)
    let reset = getById<Fable.Import.Browser.HTMLButtonElement> "reset" 
    reset.addEventListener_click(fun _ -> renderInitial(); null)
    //render()
    printfn "Main code finished"
    
main()

