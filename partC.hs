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
showTape (Tape ls h rs) = reverse (take 10 ls) ++ "|" ++ [h] ++ "|" ++ (take 10 rs)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) h rs) = Tape ls l (h:rs)

moveRight :: Tape a -> Tape a
moveRight (Tape ls h (r:rs)) = Tape (h:ls) r rs

write :: Tape a -> a -> Tape a
write (Tape ls x rs) v = Tape ls v rs

runTM :: TM a s -> Tape a
runTM (TM as ss s0 t0 tf) =
    case tf s0 t0 of 
        Just(s1, t1) -> runTM (TM as ss s1 t1 tf)
        Nothing -> t0

getTapeHead :: Tape a -> a
getTapeHead (Tape ls x rs) = x

-- Get the output resulting from running the TM 
outputTM :: TM a s -> a
outputTM tm = getTapeHead (runTM tm)

getConfigsTM :: TM a s -> [(s, a, Tape a)]
getConfigsTM (TM as ss s0 t0@(Tape _ h0 _) tf) =
    (s0, h0, t0) : case tf s0 t0 of 
                Just(s1, t1) -> getConfigsTM (TM as ss s1 t1 tf)
                Nothing -> []

{- Transition function for palindrome detection
   States are Strings e.g. q0, q1, ... , qAccept, qReject
   s is the current state
   h is the input symbol (current head of the tape)
-}
palinTF :: String -> Tape Char -> Maybe (String, Tape Char)
palinTF s t@(Tape _ h _) = 
    case (s,h) of
        ("q0", '_') -> Just ("qAccept", write t 'a') -- Accept the empty string
        ("q0", 'a') -> Just ("qA1", moveRight (write t '_')) -- Read an 'a' at the start
        ("q0", 'b') -> Just ("qB1", moveRight (write t '_')) -- Read a 'b' at the start
        ("qA1", '_') -> Just ("qAccept", write t 'a') -- Accept if input is just 'a'
        ("qA1", 'a') -> Just ("qA2", moveRight t)
        ("qA1", 'b') -> Just ("qA2", moveRight t)
        ("qA2", 'a') -> Just ("qA2", moveRight t)
        ("qA2", 'b') -> Just ("qA2", moveRight t)
        ("qA2", '_') -> Just ("qA3", moveLeft t)
        ("qA3", 'b') -> Just ("qReject", write t 'b')
        ("qA3", 'a') -> Just ("qC", moveLeft (write t '_'))
        ("qA3", '_') -> Just ("qC", moveLeft (write t '_')) -- This situation won't arise since we have just moved left from the first '_'
        ("qC", 'a') -> Just ("qC", moveLeft t) 
        ("qC", 'b') -> Just ("qC", moveLeft t) 
        ("qC", '_') -> Just ("q0", moveRight t) -- First and last symbol have matched up so we are back at the start state
        ("qB1", '_') -> Just ("qAccept", write t 'a') -- Accept if input is just 'b'
        ("qB1", 'a') -> Just ("qB2", moveRight t)
        ("qB1", 'b') -> Just ("qB2", moveRight t)
        ("qB2", 'a') -> Just ("qB2", moveRight t)
        ("qB2", 'b') -> Just ("qB2", moveRight t)
        ("qB2", '_') -> Just ("qB3", moveLeft t)
        ("qB3", 'a') -> Just ("qReject", write t 'b')
        ("qB3", 'b') -> Just ("qC", moveLeft (write t '_'))
        ("qB3", '_') -> Just ("qC", moveLeft (write t '_')) -- This situation won't arise since we have just moved left from the first '_'
        ("qAccept", 'a') -> Nothing -- Halt the TM, since we have accepted
        ("qReject", 'b') -> Nothing -- Halt the TM, since we have rejected


-- Infinite list of blank symbols for the left and right of the tape
blanks :: [Char]
blanks = repeat '_' 

tPal1, tPal2, tPal3, tPal4 :: Tape Char
tPal1 = Tape blanks 'a' ("bba" ++ blanks)
tPal2 = Tape blanks 'b' ("aabaab" ++ blanks)
tPal3 = Tape blanks '_' blanks
tPal4 = Tape blanks 'b' blanks
tPal5 = Tape blanks 'a' ("bab" ++ blanks)

tPalTest :: Tape Char
tPalTest =  Tape blanks 'a' ("ba" ++ blanks)

tmPal :: Tape Char -> TM Char String
tmPal t = TM ['-','a','b'] ["q0", "qA1", "qA2", "qA3", "qB1", "qB2", "qB3", "qC", "qAccept", "qReject"] "q0" t palinTF

-- Outputs the TM configurations in the required format
showConfigs :: TM Char String -> IO ()
showConfigs tm = sequence_ (map (\(s,i,t)-> putStrLn ("<" ++ show s ++ ", " ++ show i ++ ", " ++ showTape t ++ ">"))
                    (getConfigsTM tm))

-- Creates a tape using the given input string
generateTape :: String -> Tape Char
generateTape (l:rs) = Tape blanks l (rs ++ blanks) 

-- Takes in a string of a's and b's
-- Returns a if it is a palindrome, b otherwise
checkPalindrome :: String -> Char
checkPalindrome s = outputTM (tmPal (generateTape s))