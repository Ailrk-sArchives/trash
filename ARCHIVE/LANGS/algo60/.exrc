echo "hi"
autocmd BufWritePost * :!runhaskell Clean.hs
