/// This module runs on client and contains interfaces to server-side
/// electron code that has full access to native resources
module App.Renderer
open Fable.Import
open Fable.Core
open Fable.Core.JsInterop

(* 
    Electron automatically translates client-side functions into IPCs
    to the server so that they work as expected. The Remote module does
    this but it happens implicitly with fs module

    Reference APIs:
    electron.Dialog: https://github.com/electron/electron/blob/master/docs/api/dialog.md
    Node.fs: https://nodejs.org/api/fs.html
    Electron.Remote: https://github.com/electron/electron/blob/master/docs/api/remote.md
*)
 

/// JS function used to access Node modules from client code
[<Global>]
let require: string->obj  =  jsNative

/// Electron object
let electron  = require("electron") :?> Fable.Import.Electron.ElectronMainAndRenderer

/// remote module allows access to native resources from renderer process
/// electron.remote is like electron but includes server-side functions
/// which are called via IPC automatically

let remote = electron.remote

/// Native Filesystem from Node.fs see Node fs module documentation
/// Type info displayed by intellisense and used by editor is inexact
/// because overloads are not allowed. To use overloaded versions
/// of API functions electronFS?funcName(paras) will allow any type of inputs
/// :?> on output will cast this to correct type
/// see below for examples
let electronFS  = require("fs")  :?> Node.fs_types.Globals
(* 
    NB fs cannot be included in the rolled up code made by FABLE because
    it uses electron internal functions. A require('fs') script in the HTML before
    the FABLE-generated code allows this. See index.html.
 *)     

/// access to native dialogs
let dialog = remote.dialog

// acces to main process browserwindow
let browserWindow = remote.BrowserWindow


// helper types to interface to JS dialogs
// options are passed as objects with optional fields
// one for each possible option type
// This is a special FABLE type representing such an object
// See below for FABLE object literal syntax

[<KeyValueList>]
type OpenDialogOptions =
    | Title of string
    | DefaultPath of string
    | Filters of string
    | Properties of array<string>

[<KeyValueList>]
type SaveDialogOptions =
    | Title of string
    | DefaultPath of string
    | Filters of string

// File manipulation commands are accessed from renderer 
// via remote remote IPCs which are inserted automatically
// See Node.js fs module documentation.
// fs is a member of fs_types imported from Fable.Import.Node
// however the fsharp type of ElectronFS contains only one 
// overload of functions so
// dynamic access (ignoring type system) is sometimes needed
    
let readFileSimple (fName:string) =
    try
    // use dynamic typing to access overloaded function
    // use type cast to turn output into what it should be
    // return optional string read on success, or None on
    // failure
        electronFS?readFileSync(fName, "utf8") :?> string |> Some
    with
    | _ -> None

/// write file fName with string data  
/// return false if file already exists  
let writeFileSimple (fName:string) (data:string) =
    try
        // use dynamic typing for interface to JS because
        // this function has multiple overloads and FS
        // only understands one of them
        electronFS?writeFileSync(fName, data) |> ignore
        true // write succeeded, so return true
    with
        | e -> printfn "%A" e ; false // write failed, return false


/// Return (file name, file data) as Option from a file open dialog
/// or None if open operation is cancelled or file read fails
let readFromFileDialog fs = 
    let chooseFileOptions =
        [
            Properties  [|"openFile"|]
        ] 
    let optFName = 
        match dialog?showOpenDialog( chooseFileOptions) with
        | :? (string []) as x ->  Some x.[0]
        | _ -> None
    match optFName with
    | Some fn ->
        match readFileSimple fn with
        | Some data -> Some (fn,data)
        | _ -> None
    | _ -> None

/// save data to a file whose name is selected from dialog
/// return Some name, or None of operation fails
let writeFromFileDialog(data: string) = 
    let chooseFileOptions = [
            Title "my dialog"
            DefaultPath  @"C:\tmp"
        ] 
    let optFName = 
        match dialog?showSaveDialog( chooseFileOptions) with
        | :? (string []) as x ->  Some x.[0]
        | _ -> None
    match optFName with
    | Some fn ->  if writeFileSimple fn data then Some fn else None
    | None -> None



 

/// This function demonstrates access to server Node API as defined in 
/// Fable.Import.Node and Fable.Import.Electron
/// This allows a client (web-page) app full access to a computer which it
/// runs on using electron instead of a browser.
let render() =
    // demo how to use Electron dialog boxes
    let showMessage s =
        let showMessageOptions (s:string) =
            // creates an literal object for JS interface
            // the string/value pairs here make object properties
            // 
            createObj [ 
                "buttons" ==> [|"OK";"Terrible"|]
                "title"==> "My Box"
                "message" ==>  s; 
                "filters" ==> None; 
                "properties" ==> Some [|"openFile"|]
                // :?> casts obj type to correct type to fit JS
                // interface. TODO - chnage this to KeyValueList
            ] :?> Electron.ShowMessageBoxOptions
        let ret = dialog.showMessageBox(showMessageOptions s)
        printfn "Messagebox returned %f" ret

    /// demo how to read a file
    /// displays a box with file length
    match readFromFileDialog() with
    | Some (fName,data) ->
        printfn "File %s has size %d bytes" fName (data.Length)
        showMessage (sprintf "File %s has length %d" fName data.Length)
    | None -> printfn "No file name given!"

    

