module Basic where

-- parser is like a state
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

char :: Char -> Parser Char
char c =
  Parser $ \s ->
    case s of
      (x : xs) -> if c == x then Just (c, xs) else Nothing
      _ -> Nothing

-- Hutton Meijer parser
-- it express a range of possible valid parses starting from the input provided.
type Token = Char

newtype HMParser a = P {runHMParser :: [Token] -> [(a, [Token])]}
