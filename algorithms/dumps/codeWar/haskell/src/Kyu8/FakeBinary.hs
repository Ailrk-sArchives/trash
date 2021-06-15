module Kyu8.FakeBinary where
-- Given a string of digits, you should replace any digit below 5 with '0' and any digit 5 and above with '1'.
-- Return the resulting string.

fakeBin :: String -> String
fakeBin xs = f <$> xs
  where
    f x | (read (x : "") :: Integer) < 5 = '0'
      | otherwise = '1'

fakeBin' :: String -> String
fakeBin' = fmap (\c -> if c < '5' then '0' else '1')
