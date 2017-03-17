let rec createWordsFromBytes (myList:int list) =
  match myList with
  | a :: b :: c :: d :: t -> a + (b <<< 8) + (c <<< 16) + (d<<<24) :: createWordsFromBytes t
  | a :: b :: [c] -> [a + (b <<< 8) + (c <<< 16)]
  | a :: [b] ->  [a + (b <<< 8)]
  | [a] -> [a]
  | [] -> []
