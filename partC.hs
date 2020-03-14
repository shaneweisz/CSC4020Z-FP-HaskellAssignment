{-  A Tape consists of:
        1) The head - the symbol it's pointing to
        2) A list of symbols on the left of the head
        3) A list of symbols on the right of the head
    The type a will typically be Char
-}
data Tape a = Tape [a] a [a] 

{-  A TM consists of:
        1) The alphabet symbols
        2) The list of states states
        3) The start state
        4) The starting tape
        5) The transition function
    The type a will typically be Char
-}
data TM a s = TM [a] [s] s (Tape a) (s -> Tape a -> Maybe (s, Tape a))

-- Display Tapes consisting of Char symbols
showTape :: Tape Char -> String
showTape (Tape ls h rs) = (take 10 (reverse ls)) ++ "|" ++ [h] ++ "|" ++ (take 10 rs)
-- e.g. showTape (Tape ['2','1'] '3' ['4', '5'])

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) h rs) = Tape ls l (h:rs)
-- e.g. showTape (moveLeft (Tape ['2','1'] '3' ['4', '5']))

moveRight :: Tape a -> Tape a
moveRight (Tape ls h (r:rs)) = Tape (h:ls) r rs

write :: Tape a -> a -> Tape a
write (Tape ls x rs) v = Tape ls v rs

runTM :: TM a s -> Tape a
runTM (TM as ss s0 t0 tf) =
    case tf s0 t0 of 
        Just(s1, t1) -> runTM (TM as ss s1 t1 tf)
        Nothing -> t0

showConfigsTM :: TM a s -> [(s, Tape a)]
showConfigsTM (TM as ss s0 t0 tf) =
    (s0, t0) : case tf s0 t0 of 
                Just(s1, t1) -> showConfigsTM (TM as ss s1 t1 tf)
                Nothing -> []

blanks :: [Char]
blanks = repeat '_' -- Infinite list of blank symbols for the left and right of the tape

tPal1, tPal2, tPal3, tPal4 :: Tape Char
tPal1 = Tape blanks 'a' ("bba" ++ blanks)
tPal2 = Tape blanks 'b' ("aabaab" ++ blanks)
tPal3 = Tape blanks '_' blanks
tPal4 = Tape blanks 'b' blanks
tPal5 = Tape blanks 'a' ("bab" ++ blanks)

tPalTest :: Tape Char
tPalTest =  Tape blanks 'a' ("ba" ++ blanks)

-- tmPal :: Tape Char -> TM Char Int
-- tmPal t = TM " abYN" [0,10,11,12,20,21,22,30] 0 t palin