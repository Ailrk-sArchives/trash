module Parse where


import Control.Monad.State

type Parser a = StateT String [] (Maybe a)
data EOF = EOF

stop :: Parser a
stop = state $ \s -> error "stopped"

eof :: Parser a
eof = state $ \s -> (Nothing, "")

char :: Char -> Parser Char
char c = state $ \s ->
  case s of
    (x:xs) -> if c == x then (Just c, xs) else (Nothing, s)
    _ -> (Nothing, s)

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop



