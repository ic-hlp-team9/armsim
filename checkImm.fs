let checkImmediate (num:int) =
  let rotateRight n = int ((uint32 n) >>> 1 ||| ((uint32 n &&& 1u) <<< 31))
  let rec chIm im rotNum valid = 
    match rotNum with 
    | 0 -> valid 
    | _ -> chIm (rotateRight im) (rotNum-1) (valid || (im&&&(~~~255) = 0))
  chIm num 31 false

