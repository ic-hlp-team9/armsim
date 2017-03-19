let intToHexString n = 
    let mapToString = function 
    | 10u -> "A"
    | 11u -> "B"
    | 12u -> "C"
    | 13u -> "D"
    | 14u -> "E"
    | 15u -> "F"
    | x -> string(x)
    List.fold (fun (myNum, myStr) thisShift -> (myNum, mapToString ((myNum >>> thisShift) &&& 15u) + myStr) ) ((uint32 n), "") [0..4..28] |> snd
let checkImmediate (num:int) =
  let rotateRight n = int ((uint32 n) >>> 1 ||| ((uint32 n &&& 1u) <<< 31))
  let rec chIm im rotNum valid = 
    match rotNum with 
    | 0 -> valid 
    | _ -> chIm (rotateRight im) (rotNum-1) (valid || (im&&&(~~~255) = 0))
  chIm num 31 false

