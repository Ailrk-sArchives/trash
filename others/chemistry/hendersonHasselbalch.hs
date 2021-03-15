module HendersonHasselbalch where

-- ph and pka

-- - conjugate acid and conjugate base.
--   conjugate acid: being able to give away proton.
--   conjugate base: being able to take proton.
--   e.g HF/F- is a conjugate pair.

-- - for buffer solution we want the ph to be stable at certain ph.
--   solution with conjugate pair can rebalance the ph when acid or base is
--   added into the solution.

log10 = logBase 10.0

ka :: [Float] -> [Float] -> Float
ka reactants products = product reactants / product products

-- some definitional equations.
ph' :: Float -> Float
ph' h3o = - (log10 h3o)

pka' :: Float -> Float -> Float -> Float
pka' h3o conjugateBase conjugateAcid = ph' h3o - log10 (conjugateBase / conjugateAcid)

-- HendersonHasselbalch
-- calculate ph from pka and log of the ratio of molarity of conjugat pair

ph :: Float -> Float -> Float -> Float
ph pka conjugateBase conjugateAcid = pka + log10 (conjugateBase / conjugateAcid)

-- Thymol Blue
-- H2Ln + H20 <=> H3O+ + Ln-
-- H2Ln -> red
-- HLn- -> yellow
-- Ln2- -> blue

-- Beer's law
-- A = εcl
-- where ε: molar absorbance coefficient
--       c: concentration of sample
--       l: path length

