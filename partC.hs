data Tape a = Tape [a] a [a] 

-- Display Tapes consisting of Char symbols
showTape :: Tape Char -> String
showTape (Tape ls h rs) = (take 10 (reverse ls)) ++ "|" ++ [h] ++ "|" ++ (take 10 rs)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) h rs) = Tape ls l (h:rs)

moveRight :: Tape a -> Tape a
moveRight (Tape ls h (r:rs)) = Tape (h:ls) r rs

write :: Tape a -> a -> Tape a
write (Tape ls x rs) v = Tape ls v rs

-- data TM a s = TM [a] [s] s (Tape a) (s -> Tape a -> Maybe (s, Tape a))

-- -- runTM :: TM a s -> Tape a
-- -- runTM (TM as ss s0 t0 tf)
-- --     |