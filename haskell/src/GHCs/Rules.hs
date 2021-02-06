{-# OPTIONS_GHC -fenable-rewrite-rules #-}
module GHCs.Rules where

{-@ Rewrite rules, helps you to write specfic optimization.
    use --ddump-simpl-stats to see what rules fired.
    more details with --ddump-rule-firings.
    --ddump-rule-rewrites shows you what does the code look like
    before and after rewrite.
@-}


