{-  A Tape consists of:
        1) A list of symbols on the left of the head
        2) The head - the symbol the tape is pointing to
        3) A list of symbols on the right of the head
    The type a will typically be Char -}
data Tape a = Tape [a] a [a] 

{-  A TM consists of:
        1) The alphabet symbols
        2) The list of states states
        3) The starting state
        4) The starting tape
        5) The transition function -}
data TM a s = TM [a] [s] s (Tape a) (s -> Tape a -> Maybe (s, Tape a))

-- Display a Tape made up of Char symbols
showTape :: Tape Char -> String
showTape (Tape ls h rs) = reverse (take 10 ls) ++ "|" ++ [h] ++ "|" ++ (take 10 rs)

-- Moves the head of the Tape one symbol to the left
moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) h rs) = Tape ls l (h:rs)

-- Moves the head of the Tape one symbol to the right
moveRight :: Tape a -> Tape a
moveRight (Tape ls h (r:rs)) = Tape (h:ls) r rs

-- Writes a new value at the head of the Tape
write :: Tape a -> a -> Tape a
write (Tape ls x rs) v = Tape ls v rs

-- Runs a TM until it halts (the transition function returns Nothing), and then returns the final Tape
runTM :: TM a s -> Tape a
runTM (TM as ss s0 t0 tf) =
    case tf s0 t0 of 
        Nothing -> t0
        Just(s1, t1) -> runTM (TM as ss s1 t1 tf)
        
-- Extract the value at the head of the Tape
getTapeHead :: Tape a -> a
getTapeHead (Tape ls x rs) = x

-- Get the output resulting from running the TM 
outputTM :: TM a s -> a
outputTM tm = getTapeHead (runTM tm)

{- Transition function for palindrome detection
   States are Strings e.g. q0, q1, ... , qAccept, qReject
   s is the current state
   h is the input symbol (current head of the tape) -}
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

-- Returns a tape using the given input string
-- The tape points to the first symbol in the input string
--  and has blanks on the left and right of the string 
generateTape :: String -> Tape Char
generateTape (l:rs) = Tape blanks l (rs ++ blanks) 

-- Creates a TM using the palindrome transition function and an inputted tape
palinTM :: Tape Char -> TM Char String
palinTM t = TM ['_','a','b'] ["q0", "qA1", "qA2", "qA3", "qB1", "qB2", "qB3", "qC", "qAccept", "qReject"] "q0" t palinTF

-- Takes in a string of a's and b's
-- Returns 'a' if it is a palindrome, 'b' otherwise
checkPalindrome :: String -> Char
checkPalindrome s = outputTM (palinTM (generateTape s))

-- Gets the next state based on the transition function
-- s is the current state
-- h is the symbol at the head of the tape is
getNextState :: String -> Char -> String
getNextState s h = 
    case (s,h) of
        ("q0", '_') -> "qAcc ept"
        ("q0", 'a') -> "qA1"
        ("q0", 'b') -> "qB1"
        ("qA1", '_') -> "qAccept"
        ("qA1", 'a') -> "qA2"
        ("qA1", 'b') -> "qA2"
        ("qA2", 'a') -> "qA2"
        ("qA2", 'b') -> "qA2"
        ("qA2", '_') -> "qA3"
        ("qA3", 'b') -> "qReject"
        ("qA3", 'a') -> "qC"
        ("qA3", '_') -> "qC"
        ("qC", 'a') -> "qC"
        ("qC", 'b') -> "qC"
        ("qC", '_') -> "q0"
        ("qB1", '_') -> "qAccept"
        ("qB1", 'a') -> "qB2"
        ("qB1", 'b') -> "qB2"
        ("qB2", 'a') -> "qB2"
        ("qB2", 'b') -> "qB2"
        ("qB2", '_') -> "qB3"
        ("qB3", 'a') -> "qReject"
        ("qB3", 'b') -> "qC"
        ("qB3", '_') -> "qC"
        ("qAccept", 'a') -> "qAccept"
        ("qReject", 'b') -> "qAccept"

-- Gets the next symbol to be written based on the transition function
-- s is the current state
-- h is the symbol at the head of the tape is
getWriteSymbol :: String -> Char -> Char
getWriteSymbol s h =
    case (s,h) of
    ("q0", '_') -> 'a'
    ("q0", 'a') -> '_'
    ("q0", 'b') -> '_'
    ("qA1", '_') -> 'a'
    ("qA1", 'a') -> 'a'
    ("qA1", 'b') -> 'b'
    ("qA2", 'a') -> 'a'
    ("qA2", 'b') -> 'b'
    ("qA2", '_') -> '_'
    ("qA3", 'b') -> 'b'
    ("qA3", 'a') -> '_'
    ("qA3", '_') -> '-'
    ("qC", 'a') -> 'a'
    ("qC", 'b') -> 'b'
    ("qC", '_') -> '_'
    ("qB1", '_') -> 'a'
    ("qB1", 'a') -> 'a'
    ("qB1", 'b') -> 'b'
    ("qB2", 'a') -> 'a'
    ("qB2", 'b') -> 'b'
    ("qB2", '_') -> '_'
    ("qB3", 'a') -> 'b'
    ("qB3", 'b') -> '_'
    ("qB3", '_') -> '_'
    ("qAccept", 'a') -> 'a'
    ("qReject", 'b') -> 'b'

-- Get the list of TM configurations in computing the palindrome check function
getConfigsTM :: TM Char String -> [(String, Char, Char, String)]
getConfigsTM (TM as ss s0 t0@(Tape _ h0 _) tf) =
    (s0, h0, getWriteSymbol s0 h0, getNextState s0 h0) : case tf s0 t0 of 
                Nothing -> []
                Just(s1, t1) -> getConfigsTM (TM as ss s1 t1 tf)

-- Outputs the list of TM configurations in computing the palindrome check function, in the following format:
-- < CurrentState, InputSymbol, OutputSymbol, NewState >
showConfigs :: TM Char String -> IO ()
showConfigs tm = sequence_ (map (\(s,i,j,n)-> putStrLn ("< " ++ show s ++ ", " ++ show i ++ ", " ++ show j ++ ", " ++ show n ++ " >"))
                    (init (getConfigsTM tm)))             

