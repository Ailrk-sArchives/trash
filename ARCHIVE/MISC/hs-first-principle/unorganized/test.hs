sayHello :: String -> IO()
sayHello x = putStrLn ("Hello" ++ x ++ "!")

-- modulo consistency
(quot x y) * x + (rem x y) == x
(div x y) * x + (mod x y) == x


