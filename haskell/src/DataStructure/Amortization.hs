{-# LANGUAGE TypeFamilies #-}
module DataStructure.Amortization where

import Test.Hspec

-------------------------------------------------------------------------------
-- Traditional Amortization
-- Worst case analysis assume the worst senario for the algorithm, but it might
-- quite unlikely, or sometimes impossible to hit the worst case.
--
-- Amortization help us to analyse the overall running time. Given a sequence
-- of operations, we care about the running time over the entire sequence
-- instead of any individual operation. With amortization we allow occasional
-- expensive operations as long as the amortized running time is bounded.
--
-- A canonical example is table resizing.
--
-- -------
-- How to prove amortized time bound?
--
-- sum(a[i] | i=[1..m]) >= sum(t[i] | i=[1..m])
--    where a is the amortized cost for each operation,
--          t is the actual cost for each operation.
--
-- It's a comparison of the summation of runing time.
--
-- Also we define accumulated saving as:
--  c = sum(a[i] | i=[1..m]) - sum(t[i] | i=[1..m])
--  thus if c > 0 we proved the amortized bound.
--
-- -------
-- For amortized bound O(X),
-- an operation with actual cost <= O(X) is cheap
-- an operation with actual cost > O(X) is expensive

-------------------------------------------------------------------------------
-- Banker's method (Accountant's method)
-- - opertaions can store credit                    (deposit)
-- - operation can pay for time using credit        (withdraw)
-- - amortized cost calulation:
--   - ((amortized cost = actual cost + deposits - withdraw))
--
-- The idea is that the more cheap operations we perform, the more we can
-- afford for expensive operation. If we store credits for each operation,
-- we know exactly how many expensive operations we can afford.

amortizedCostBanker cost deposit withdraw = cost + deposit - withdraw
-- ------------
-- e.g 2 3 tree
--   c = O(1) worst case creating empty tree
--   i = O(lgn*) amortized/real time for insert x  |credit = O(lgn*), withdraw = 0
--   d = O(lgn*) real time delete x                |credit = 0, withdraw = O(lgn*)
--       :amortized cost = O(lgn*) - O(lgn*) = O(0)
--
--
--   We known we can only delete elements that're already inserted, so
--   i >= d
--
--   total cost = O(c + ilgn* + dlgn*)   -- d <= i
--              => O(c + ilgn*) + d.0
--
-- ------------
-- e.g table doubling  assume table with size n and only insert.
--   (when elements in the table > n /2), double table size n
--   c = O(1) insertion         |credit = 1 for the item
--       :amortized cost = O(1)
--
--       O(n) double table      |credit = O(n/2)
--       :amortized cost = O(n) - c*(n/2) = 0 if c larget
--
-- NOTE: another perspective is to think whenever we need to double the table,
--       we need to charge credicts from the bank.
--       Each insertion will add credit into the bank so we can afford the
--       charge.
--
-- ------------
-- e.g tabel double and halving
--   c = actual O(1) insertion
--   d = actual O(1) deletion
--
--   O(n) double table  100% full      |credit = O(n/2)
--      :amortized O(n) - c*(n/2)
--   O(n) halve table   25% full       |credit = O(n/4)
--      :amortized O(n) - c*(n/4)
--
-- NOTE: with the above criterion, everytime after doubling or halving, the
--       table is 50% full.
--
-- NOTE: Meaning after we doubled, if we want to halve, we need to delete at
--       least 25% elements. By that time we we've accumulate enough credicts
--       to pay for that.
--       The same analysis apply for halve and doubling.

-------------------------------------------------------------------------------
-- Physicist's method (Potential method)
-- define potential function phi : data structure configuration -> uint
-- measures how bad the current configuration is.

class Configuration a where
  configuration :: a -> a

-- we need to define our own phi function for each data sturcture
phi :: Configuration a => Num b => a -> b
phi = undefined

amortizedCostPhycist :: Configuration b => Num a => a -> b -> b -> a
amortizedCostPhycist cost after before = cost + phi(after) - phi(before)

---------------
-- e.g cost of incrementing binary counter with potential method
--     inc(0011010111) -> 0011011000
-- NOTE: in a good case we can just replace a zero with one
--       in a bad case we need to replace t 1s.
--       when all numbers are 1, the actual cost will be O(n)
--       what's the amortized cost of increment?
--
-- actual cost = 1 + t
--    where t = (# of trailing 1s)
--
-- we first need to define the phi function
-- let phi = # of 1s in the binary number
--    - when there is no trailing bits, we increase the potential by 1
--    - when there are t trailing bits, we destroys t 1s and create 1
-- this means, the more 1 there are, the more potential the data structure gets
-- If phi(after) < phi(before), we're making the configuration worse.
-- If phi(after) > phi(before), the configuration is getting better.
-- If we make configuration worse for one operation, we need to have enough cost
--    to compensate that.
--
-- amortized cost for increment:
-- amortized cost = 1 + t - t + 1 = 2

---------------
-- Analyse insertion in 2-3 three
-- how many splits per insert?
--  Worst case # of splits: O(logn)
--  Amortized case # of splits: O(1)
--
--
