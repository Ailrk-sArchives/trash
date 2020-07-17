{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Data.Nat.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra
import qualified MAlonzo.Code.Algebra.Morphism
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Bool.Properties
import qualified MAlonzo.Code.Data.Empty
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Injection
import qualified MAlonzo.Code.Relation.Binary
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Core
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Nullary
import qualified MAlonzo.Code.Relation.Nullary.Decidable

name8 = "Data.Nat.Properties._._Absorbs_"
d8 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d8 = erased
name10 = "Data.Nat.Properties._._DistributesOver_"
d10 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d10 = erased
name12 = "Data.Nat.Properties._._DistributesOverʳ_"
d12 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d12 = erased
name14 = "Data.Nat.Properties._._DistributesOverˡ_"
d14 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d14 = erased
name18 = "Data.Nat.Properties._.Absorptive"
d18 ::
  (Integer -> Integer -> Integer) ->
  (Integer -> Integer -> Integer) -> ()
d18 = erased
name20 = "Data.Nat.Properties._.Associative"
d20 :: (Integer -> Integer -> Integer) -> ()
d20 = erased
name22 = "Data.Nat.Properties._.Commutative"
d22 :: (Integer -> Integer -> Integer) -> ()
d22 = erased
name30 = "Data.Nat.Properties._.Idempotent"
d30 :: (Integer -> Integer -> Integer) -> ()
d30 = erased
name34 = "Data.Nat.Properties._.Identity"
d34 :: Integer -> (Integer -> Integer -> Integer) -> ()
d34 = erased
name44 = "Data.Nat.Properties._.LeftIdentity"
d44 :: Integer -> (Integer -> Integer -> Integer) -> ()
d44 = erased
name48 = "Data.Nat.Properties._.LeftZero"
d48 :: Integer -> (Integer -> Integer -> Integer) -> ()
d48 = erased
name54 = "Data.Nat.Properties._.RightIdentity"
d54 :: Integer -> (Integer -> Integer -> Integer) -> ()
d54 = erased
name58 = "Data.Nat.Properties._.RightZero"
d58 :: Integer -> (Integer -> Integer -> Integer) -> ()
d58 = erased
name60 = "Data.Nat.Properties._.Selective"
d60 :: (Integer -> Integer -> Integer) -> ()
d60 = erased
name62 = "Data.Nat.Properties._.Zero"
d62 :: Integer -> (Integer -> Integer -> Integer) -> ()
d62 = erased
name74 = "Data.Nat.Properties._.IsBand"
d74 a0 = ()
name80 = "Data.Nat.Properties._.IsCommutativeMonoid"
d80 a0 a1 = ()
name84 = "Data.Nat.Properties._.IsCommutativeSemiring"
d84 a0 a1 a2 a3 = ()
name86 = "Data.Nat.Properties._.IsCommutativeSemiringWithoutOne"
d86 a0 a1 a2 = ()
name88 = "Data.Nat.Properties._.IsDistributiveLattice"
d88 a0 a1 = ()
name94 = "Data.Nat.Properties._.IsLattice"
d94 a0 a1 = ()
name96 = "Data.Nat.Properties._.IsMagma"
d96 a0 = ()
name98 = "Data.Nat.Properties._.IsMonoid"
d98 a0 a1 = ()
name106 = "Data.Nat.Properties._.IsSemigroup"
d106 a0 = ()
name108 = "Data.Nat.Properties._.IsSemilattice"
d108 a0 = ()
name110 = "Data.Nat.Properties._.IsSemiring"
d110 a0 a1 a2 a3 = ()
name114 = "Data.Nat.Properties._.IsSemiringWithoutOne"
d114 a0 a1 a2 = ()
name1240 = "Data.Nat.Properties.suc-injective"
d1240 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1240 = erased
name1246 = "Data.Nat.Properties.≡ᵇ⇒≡"
d1246 ::
  Integer ->
  Integer -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1246 = erased
name1258 = "Data.Nat.Properties.≡⇒≡ᵇ"
d1258 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1258 = erased
name1268 = "Data.Nat.Properties._≟_"
d1268 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d1268 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du24 erased erased
      (MAlonzo.Code.Data.Bool.Properties.d1928
         (coe (eqInt (coe v0) (coe v1))))
name1274 = "Data.Nat.Properties.≡-irrelevant"
d1274 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1274 = erased
name1282 = "Data.Nat.Properties.≟-diag"
d1282 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1282 = erased
name1284 = "Data.Nat.Properties.≡-isDecEquivalence"
d1284 :: MAlonzo.Code.Relation.Binary.T174
d1284
  = coe
      (MAlonzo.Code.Relation.Binary.C1313
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         (coe d1268))
name1286 = "Data.Nat.Properties.≡-decSetoid"
d1286 :: MAlonzo.Code.Relation.Binary.T206
d1286
  = coe
      (\ v0 v1 v2 -> MAlonzo.Code.Relation.Binary.C1455 v2) erased erased
      d1284
name1290 = "Data.Nat.Properties.1+n≢0"
d1290 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d1290 = erased
name1296 = "Data.Nat.Properties.<ᵇ⇒<"
d1296 ::
  Integer -> Integer -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T6
d1296 v0 v1 v2 = du1296 v0
du1296 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1296 v0
  = case coe v0 of
      0 -> coe
             (\ v1 v2 v3 -> MAlonzo.Code.Data.Nat.Base.C18 v3) erased erased
             (coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C10) erased)
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 v4 -> MAlonzo.Code.Data.Nat.Base.C18 v4) erased erased
             (du1296 (coe v1))
name1312 = "Data.Nat.Properties.<⇒<ᵇ"
d1312 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6 -> AgdaAny
d1312 = erased
name1320 = "Data.Nat.Properties.≤-pred"
d1320 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1320 v0 v1 v2 = du1320 v2
du1320 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1320 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C18 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1324 = "Data.Nat.Properties.≤-reflexive"
d1324 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d1324 v0 v1 v2 = du1324 v0
du1324 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1324 v0
  = case coe v0 of
      0 -> coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 v4 -> MAlonzo.Code.Data.Nat.Base.C18 v4) erased erased
             (du1324 (coe v1))
name1328 = "Data.Nat.Properties.≤-refl"
d1328 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
d1328 v0 = coe (du1324 (coe v0))
name1330 = "Data.Nat.Properties.≤-antisym"
d1330 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1330 = erased
name1336 = "Data.Nat.Properties.≤-trans"
d1336 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1336 v0 v1 v2 v3 v4 = du1336 v3 v4
du1336 ::
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1336 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> coe (\ v3 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      MAlonzo.Code.Data.Nat.Base.C18 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C18 v7
               -> coe
                    (\ v8 v9 v10 -> MAlonzo.Code.Data.Nat.Base.C18 v10) erased erased
                    (du1336 (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name1342 = "Data.Nat.Properties.≤-total"
d1342 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d1342 v0 v1
  = case coe v0 of
      0 -> coe
             (MAlonzo.Code.Data.Sum.Base.C38
                (coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    (MAlonzo.Code.Data.Sum.Base.C42
                       (coe (\ v3 -> MAlonzo.Code.Data.Nat.Base.C10) erased))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  let v4 = d1342 (coe v2) (coe v3) in
                  case coe v4 of
                    MAlonzo.Code.Data.Sum.Base.C38 v5
                      -> coe
                           (MAlonzo.Code.Data.Sum.Base.C38
                              (coe
                                 (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
                                 v5))
                    MAlonzo.Code.Data.Sum.Base.C42 v5
                      -> coe
                           (MAlonzo.Code.Data.Sum.Base.C42
                              (coe
                                 (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
                                 v5))
                    _ -> MAlonzo.RTE.mazUnreachableError
name1364 = "Data.Nat.Properties.≤-irrelevant"
d1364 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1364 = erased
name1370 = "Data.Nat.Properties._≤?_"
d1370 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d1370 v0 v1
  = case coe v0 of
      0 -> coe
             (MAlonzo.Code.Relation.Nullary.C22
                (coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           let v3
                 = MAlonzo.Code.Data.Bool.Properties.d1928
                     (coe (ltInt (coe v2) (coe v1))) in
           case coe v3 of
             MAlonzo.Code.Relation.Nullary.C22 v4
               -> coe (MAlonzo.Code.Relation.Nullary.C22 (coe (du1296 (coe v2))))
             MAlonzo.Code.Relation.Nullary.C26
               -> coe (\ v5 -> MAlonzo.Code.Relation.Nullary.C26) erased
             _ -> MAlonzo.RTE.mazUnreachableError
name1392 = "Data.Nat.Properties._≥?_"
d1392 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d1392 v0 v1 = coe (d1370 (coe v1) (coe v0))
name1394 = "Data.Nat.Properties.≤-isPreorder"
d1394 :: MAlonzo.Code.Relation.Binary.T16
d1394
  = coe
      (MAlonzo.Code.Relation.Binary.C17
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         (coe d1324) (coe d1336))
name1396 = "Data.Nat.Properties.≤-isPartialOrder"
d1396 :: MAlonzo.Code.Relation.Binary.T256
d1396 = coe (MAlonzo.Code.Relation.Binary.C1639 (coe d1394) erased)
name1398 = "Data.Nat.Properties.≤-isTotalOrder"
d1398 :: MAlonzo.Code.Relation.Binary.T876
d1398
  = coe (MAlonzo.Code.Relation.Binary.C5461 (coe d1396) (coe d1342))
name1400 = "Data.Nat.Properties.≤-isDecTotalOrder"
d1400 :: MAlonzo.Code.Relation.Binary.T1010
d1400
  = coe
      (MAlonzo.Code.Relation.Binary.C6205
         (coe d1398) (coe d1268) (coe d1370))
name1402 = "Data.Nat.Properties.≤-preorder"
d1402 :: MAlonzo.Code.Relation.Binary.T74
d1402
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C807 v3) erased
      erased erased d1394
name1404 = "Data.Nat.Properties.≤-poset"
d1404 :: MAlonzo.Code.Relation.Binary.T310
d1404
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C1973 v3) erased
      erased erased d1396
name1406 = "Data.Nat.Properties.≤-totalOrder"
d1406 :: MAlonzo.Code.Relation.Binary.T934
d1406
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C5847 v3) erased
      erased erased d1398
name1408 = "Data.Nat.Properties.≤-decTotalOrder"
d1408 :: MAlonzo.Code.Relation.Binary.T1084
d1408
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C6763 v3) erased
      erased erased d1400
name1418 = "Data.Nat.Properties.s≤s-injective"
d1418 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1418 = erased
name1424 = "Data.Nat.Properties.≤-step"
d1424 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1424 v0 v1 v2 = du1424 v2
du1424 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1424 v0 = coe v0
name1430 = "Data.Nat.Properties.n≤1+n"
d1430 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
d1430 v0 = coe (d1328 (coe v0))
name1434 = "Data.Nat.Properties.1+n≰n"
d1434 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1434 = erased
name1440 = "Data.Nat.Properties.n≤0⇒n≡0"
d1440 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1440 = erased
name1442 = "Data.Nat.Properties.<⇒≤"
d1442 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1442 v0 v1 v2 = du1442 v1 v2
du1442 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1442 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C18 v4
        -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (du1336 (coe v4) (coe (d1328 (coe v5))))
      _ -> MAlonzo.RTE.mazUnreachableError
name1446 = "Data.Nat.Properties.<⇒≢"
d1446 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d1446 = erased
name1450 = "Data.Nat.Properties.≤⇒≯"
d1450 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1450 = erased
name1456 = "Data.Nat.Properties.<⇒≱"
d1456 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1456 = erased
name1462 = "Data.Nat.Properties.<⇒≯"
d1462 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1462 = erased
name1468 = "Data.Nat.Properties.≰⇒≮"
d1468 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1468 = erased
name1474 = "Data.Nat.Properties.≰⇒>"
d1474 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d1474 v0 v1 v2 = du1474 v0 v1
du1474 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1474 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Empty.d10 () erased erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    (\ v3 v4 v5 -> MAlonzo.Code.Data.Nat.Base.C18 v5) erased erased
                    (coe (\ v3 -> MAlonzo.Code.Data.Nat.Base.C10) erased)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (du1474 (coe v2) (coe v3))
name1486 = "Data.Nat.Properties.≰⇒≥"
d1486 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d1486 v0 v1 v2 = du1486 v0 v1
du1486 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1486 v0 v1
  = coe (du1442 (coe v0) (coe (du1474 (coe v0) (coe v1))))
name1488 = "Data.Nat.Properties.≮⇒≥"
d1488 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d1488 v0 v1 v2 = du1488 v0 v1
du1488 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1488 v0 v1
  = case coe v1 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             0 -> coe MAlonzo.Code.Data.Empty.d10 () erased erased
             _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (du1488 (coe v3) (coe v2))
name1504 = "Data.Nat.Properties.≤∧≢⇒<"
d1504 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T12 ->
   MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d1504 v0 v1 v2 v3 = du1504 v1 v2
du1504 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1504 v0 v1
  = case coe v0 of
      0 -> coe
             (seq (coe v1) (coe MAlonzo.Code.Data.Empty.d10 () erased erased))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (coe (\ v4 -> MAlonzo.Code.Data.Nat.Base.C10) erased)
             MAlonzo.Code.Data.Nat.Base.C18 v5
               -> coe
                    (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
                    (du1504 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
name1518 = "Data.Nat.Properties.≤-<-connex"
d1518 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d1518 v0 v1
  = let v2 = d1370 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Relation.Nullary.C22 v3
        -> coe (MAlonzo.Code.Data.Sum.Base.C38 (coe v3))
      MAlonzo.Code.Relation.Nullary.C26
        -> coe
             (MAlonzo.Code.Data.Sum.Base.C42 (coe (du1474 (coe v0) (coe v1))))
      _ -> MAlonzo.RTE.mazUnreachableError
name1540 = "Data.Nat.Properties.≥->-connex"
d1540 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d1540 v0 v1 = coe (d1518 (coe v1) (coe v0))
name1542 = "Data.Nat.Properties.<-≤-connex"
d1542 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d1542
  = coe (MAlonzo.Code.Relation.Binary.Consequences.du782 (coe d1518))
name1544 = "Data.Nat.Properties.>-≥-connex"
d1544 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d1544
  = coe (MAlonzo.Code.Relation.Binary.Consequences.du782 (coe d1540))
name1546 = "Data.Nat.Properties.<-irrefl"
d1546 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1546 = erased
name1550 = "Data.Nat.Properties.<-asym"
d1550 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1550 = erased
name1556 = "Data.Nat.Properties.<-trans"
d1556 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1556 v0 v1 v2 v3 v4 = du1556 v1 v3 v4
du1556 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1556 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C18 v5
        -> case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C18 v8
               -> let v9 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    (\ v10 v11 v12 -> MAlonzo.Code.Data.Nat.Base.C18 v12) erased erased
                    (du1336 (coe v5) (coe (du1336 (coe (d1430 (coe v9))) (coe v8))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name1562 = "Data.Nat.Properties.<-transʳ"
d1562 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1562 v0 v1 v2 v3 v4 = du1562 v3 v4
du1562 ::
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1562 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C18 v4
        -> coe
             (\ v5 v6 v7 -> MAlonzo.Code.Data.Nat.Base.C18 v7) erased erased
             (du1336 (coe v0) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
name1568 = "Data.Nat.Properties.<-transˡ"
d1568 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1568 v0 v1 v2 v3 v4 = du1568 v3 v4
du1568 ::
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1568 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C18 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C18 v7
               -> coe
                    (\ v8 v9 v10 -> MAlonzo.Code.Data.Nat.Base.C18 v10) erased erased
                    (du1336 (coe v4) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name1574 = "Data.Nat.Properties.<-cmp"
d1574 ::
  Integer -> Integer -> MAlonzo.Code.Relation.Binary.Core.T198
d1574 v0 v1
  = let v2 = d1268 (coe v0) (coe v1) in
    let v3
          = MAlonzo.Code.Data.Bool.Properties.d1928
              (coe (ltInt (coe v0) (coe v1))) in
    case coe v2 of
      MAlonzo.Code.Relation.Nullary.C22 v4
        -> coe
             (\ v5 v6 v7 -> MAlonzo.Code.Relation.Binary.Core.C220 v6) erased v4
             erased
      MAlonzo.Code.Relation.Nullary.C26
        -> case coe v3 of
             MAlonzo.Code.Relation.Nullary.C22 v5
               -> coe
                    (\ v6 v7 v8 -> MAlonzo.Code.Relation.Binary.Core.C212 v6)
                    (du1296 (coe v0)) erased erased
             MAlonzo.Code.Relation.Nullary.C26
               -> coe
                    (\ v6 v7 v8 -> MAlonzo.Code.Relation.Binary.Core.C228 v8) erased
                    erased (du1504 (coe v0) (coe (du1488 (coe v0) (coe v1))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name1606 = "Data.Nat.Properties._<?_"
d1606 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d1606 v0 v1
  = coe (d1370 (coe (addInt (coe (1 :: Integer)) (coe v0))) (coe v1))
name1612 = "Data.Nat.Properties._>?_"
d1612 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d1612 v0 v1 = coe (d1606 (coe v1) (coe v0))
name1614 = "Data.Nat.Properties.<-irrelevant"
d1614 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1614 = erased
name1616 = "Data.Nat.Properties.<-resp₂-≡"
d1616 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d1616
  = coe
      (MAlonzo.Code.Agda.Builtin.Sigma.C32
         (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4)))
name1622 = "Data.Nat.Properties.<-isStrictPartialOrder"
d1622 :: MAlonzo.Code.Relation.Binary.T570
d1622
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C3423 v0 v2 v3)
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70 erased
      d1556 d1616
name1624 = "Data.Nat.Properties.<-isStrictTotalOrder"
d1624 :: MAlonzo.Code.Relation.Binary.T1202
d1624
  = coe
      (MAlonzo.Code.Relation.Binary.C7321
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         (coe d1556) (coe d1574))
name1626 = "Data.Nat.Properties.<-strictPartialOrder"
d1626 :: MAlonzo.Code.Relation.Binary.T628
d1626
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C4141 v3) erased
      erased erased d1622
name1628 = "Data.Nat.Properties.<-strictTotalOrder"
d1628 :: MAlonzo.Code.Relation.Binary.T1268
d1628
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C8495 v3) erased
      erased erased d1624
name1632 = "Data.Nat.Properties.n≮n"
d1632 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d1632 = erased
name1640 = "Data.Nat.Properties.m<n⇒n≢0"
d1640 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d1640 = erased
name1646 = "Data.Nat.Properties.n≢0⇒n>0"
d1646 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T12 ->
   MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d1646 v0 v1 = du1646 v0
du1646 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1646 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Empty.d10 () erased erased
      _ -> coe
             (\ v1 v2 v3 -> MAlonzo.Code.Data.Nat.Base.C18 v3) erased erased
             (coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C10) erased)
name1656 = "Data.Nat.Properties.≤-Reasoning._._<⟨_⟩_"
d1656 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1656
  = coe
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du202
         (coe d1556)
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du66)
         (coe d1568))
name1658 = "Data.Nat.Properties.≤-Reasoning._._IsRelatedTo_"
d1658 a0 a1 = ()
name1660 = "Data.Nat.Properties.≤-Reasoning._._∎"
d1660 ::
  Integer -> MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1660
  = let v0 = d1394 in
    coe
      (\ v1 ->
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.C84
           (coe
              MAlonzo.Code.Relation.Binary.Core.d414
              (MAlonzo.Code.Relation.Binary.d36 (coe v0)) v1))
name1662 = "Data.Nat.Properties.≤-Reasoning._._≈˘⟨_⟩_"
d1662 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1662
  = coe
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du280
         (coe d1394)
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du66))
name1664 = "Data.Nat.Properties.≤-Reasoning._._≡˘⟨_⟩_"
d1664 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1664 v0 v1 v2 v3 v4 = du1664 v4
du1664 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
du1664 v0 = coe v0
name1666 = "Data.Nat.Properties.≤-Reasoning._._≡⟨_⟩_"
d1666 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1666 v0 v1 v2 v3 v4 = du1666 v4
du1666 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
du1666 v0 = coe v0
name1668 = "Data.Nat.Properties.≤-Reasoning._._≡⟨⟩_"
d1668 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1668 v0 = coe v0
name1670 = "Data.Nat.Properties.≤-Reasoning._._≤⟨_⟩_"
d1670 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68
d1670
  = coe
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du228
         (coe d1394) (coe d1562))
name1672 = "Data.Nat.Properties.≤-Reasoning._.IsEquality"
d1672 a0 a1 a2 = ()
name1674 = "Data.Nat.Properties.≤-Reasoning._.IsEquality?"
d1674 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Nullary.T14
d1674
  = coe
      (\ v0 v1 v2 ->
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du140 v2)
name1676 = "Data.Nat.Properties.≤-Reasoning._.IsStrict"
d1676 a0 a1 a2 = ()
name1678 = "Data.Nat.Properties.≤-Reasoning._.IsStrict?"
d1678 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Nullary.T14
d1678
  = coe
      (\ v0 v1 v2 ->
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du106 v2)
name1680 = "Data.Nat.Properties.≤-Reasoning._.begin_"
d1680 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Data.Nat.Base.T6
d1680
  = coe
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du160
         (coe d1394) (coe d1442))
name1682 = "Data.Nat.Properties.≤-Reasoning._.begin-equality_"
d1682 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1682 = erased
name1684 = "Data.Nat.Properties.≤-Reasoning._.begin-strict_"
d1684 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  AgdaAny -> MAlonzo.Code.Data.Nat.Base.T6
d1684
  = coe
      (\ v0 v1 v2 v3 ->
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du176 v2)
name1688 = "Data.Nat.Properties.≤-Reasoning._.extractEquality"
d1688 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T124 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1688 = erased
name1690 = "Data.Nat.Properties.≤-Reasoning._.extractStrict"
d1690 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T68 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.T90 ->
  MAlonzo.Code.Data.Nat.Base.T6
d1690
  = coe
      (\ v0 v1 v2 v3 ->
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du116 v2 v3)
name1716 = "Data.Nat.Properties.pred-mono"
d1716 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1716 v0 v1 v2 = du1716 v2
du1716 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1716 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      MAlonzo.Code.Data.Nat.Base.C18 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1724 = "Data.Nat.Properties.≤pred⇒≤"
d1724 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1724 v0 v1 v2 = du1724 v1 v2
du1724 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1724 v0 v1 = coe (seq (coe v0) (coe v1))
name1740 = "Data.Nat.Properties.≤⇒pred≤"
d1740 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1740 v0 v1 v2 = du1740 v0 v2
du1740 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1740 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (du1336 (coe (d1430 (coe v2))) (coe v1))
name1752 = "Data.Nat.Properties.<⇒≤pred"
d1752 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1752 v0 v1 v2 = du1752 v2
du1752 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1752 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C18 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1758 = "Data.Nat.Properties.suc[pred[n]]≡n"
d1758 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T12 ->
   MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1758 = erased
name1770 = "Data.Nat.Properties.+-suc"
d1770 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1770 = erased
name1778 = "Data.Nat.Properties.+-assoc"
d1778 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1778 = erased
name1786 = "Data.Nat.Properties.+-identityˡ"
d1786 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1786 = erased
name1788 = "Data.Nat.Properties.+-identityʳ"
d1788 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1788 = erased
name1792 = "Data.Nat.Properties.+-identity"
d1792 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d1792 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name1794 = "Data.Nat.Properties.+-comm"
d1794 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d1794 = erased
name1802 = "Data.Nat.Properties.+-cancelˡ-≡"
d1802 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1802 = erased
name1810 = "Data.Nat.Properties.+-cancelʳ-≡"
d1810 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1810 = erased
name1812 = "Data.Nat.Properties.+-cancel-≡"
d1812 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d1812 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name1814 = "Data.Nat.Properties.+-isMagma"
d1814 :: MAlonzo.Code.Algebra.Structures.T84
d1814
  = coe
      (MAlonzo.Code.Algebra.Structures.C277
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         erased)
name1816 = "Data.Nat.Properties.+-isSemigroup"
d1816 :: MAlonzo.Code.Algebra.Structures.T118
d1816
  = coe (MAlonzo.Code.Algebra.Structures.C781 (coe d1814) erased)
name1818 = "Data.Nat.Properties.+-0-isMonoid"
d1818 :: MAlonzo.Code.Algebra.Structures.T268
d1818
  = coe
      (MAlonzo.Code.Algebra.Structures.C1873 (coe d1816) (coe d1792))
name1820 = "Data.Nat.Properties.+-0-isCommutativeMonoid"
d1820 :: MAlonzo.Code.Algebra.Structures.T314
d1820
  = coe
      (MAlonzo.Code.Algebra.Structures.C2409 (coe d1816) erased erased)
name1822 = "Data.Nat.Properties.+-magma"
d1822 :: MAlonzo.Code.Algebra.T28
d1822
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C77 v2 v3) erased erased
      addInt d1814
name1824 = "Data.Nat.Properties.+-semigroup"
d1824 :: MAlonzo.Code.Algebra.T76
d1824
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C285 v2 v3) erased erased
      addInt d1816
name1826 = "Data.Nat.Properties.+-0-monoid"
d1826 :: MAlonzo.Code.Algebra.T344
d1826
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C1365 v2 v3 v4) erased
      erased addInt (0 :: Integer) d1818
name1828 = "Data.Nat.Properties.+-0-commutativeMonoid"
d1828 :: MAlonzo.Code.Algebra.T416
d1828
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C1695 v2 v3 v4) erased
      erased addInt (0 :: Integer) d1820
name1834 = "Data.Nat.Properties.m≢1+m+n"
d1834 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d1834 = erased
name1844 = "Data.Nat.Properties.m≢1+n+m"
d1844 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d1844 = erased
name1854 = "Data.Nat.Properties.m+1+n≢m"
d1854 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d1854 = erased
name1862 = "Data.Nat.Properties.m+n≡0⇒m≡0"
d1862 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1862 = erased
name1870 = "Data.Nat.Properties.m+n≡0⇒n≡0"
d1870 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d1870 = erased
name1878 = "Data.Nat.Properties.+-cancelˡ-≤"
d1878 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1878 v0 v1 v2 v3 = du1878 v0 v3
du1878 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1878 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C18 v5 -> coe (du1878 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
name1886 = "Data.Nat.Properties.+-cancelʳ-≤"
d1886 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1886 v0 v1 v2 v3 = du1886 v0 v3
du1886 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1886 v0 v1 = coe (du1878 (coe v0) (coe v1))
name1896 = "Data.Nat.Properties.+-cancel-≤"
d1896 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d1896
  = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 (coe d1878) (coe d1886))
name1898 = "Data.Nat.Properties.+-cancelˡ-<"
d1898 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1898 v0 v1 v2 v3 = du1898 v0 v3
du1898 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1898 v0 v1 = coe (du1878 (coe v0) (coe v1))
name1908 = "Data.Nat.Properties.+-cancelʳ-<"
d1908 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1908 v0 v1 v2 v3 = du1908 v0 v3
du1908 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1908 v0 v1 = coe (du1886 (coe v0) (coe v1))
name1916 = "Data.Nat.Properties.+-cancel-<"
d1916 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d1916
  = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 (coe d1898) (coe d1908))
name1924 = "Data.Nat.Properties.≤-stepsˡ"
d1924 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1924 v0 v1 v2 v3 = du1924 v3
du1924 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1924 v0 = coe v0
name1938 = "Data.Nat.Properties.≤-stepsʳ"
d1938 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1938 v0 v1 v2 v3 = du1938 v3
du1938 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1938 v0 = coe v0
name1952 = "Data.Nat.Properties.m≤m+n"
d1952 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d1952 v0 v1 = du1952 v0
du1952 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1952 v0
  = case coe v0 of
      0 -> coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 v4 -> MAlonzo.Code.Data.Nat.Base.C18 v4) erased erased
             (du1952 (coe v1))
name1964 = "Data.Nat.Properties.m≤n+m"
d1964 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d1964 v0 v1 = du1964 v0
du1964 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du1964 v0 = coe (du1952 (coe v0))
name1978 = "Data.Nat.Properties.m+n≤o⇒m≤o"
d1978 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1978 v0 v1 v2 v3 = du1978 v0 v3
du1978 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1978 v0 v1
  = case coe v0 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C18 v5
               -> coe
                    (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
                    (du1978 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
name1992 = "Data.Nat.Properties.m+n≤o⇒n≤o"
d1992 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d1992 v0 v1 v2 v3 = du1992 v0 v2 v3
du1992 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du1992 v0 v1 v2
  = case coe v0 of
      0 -> coe v2
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (du1992 (coe v3) (coe v1) (coe (du1442 (coe v1) (coe v2))))
name2000 = "Data.Nat.Properties.+-mono-≤"
d2000 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2000 v0 v1 v2 v3 v4 v5 = du2000 v3 v4 v5
du2000 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2000 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> coe (du1336 (coe v2) (coe (du1964 (coe v0))))
      MAlonzo.Code.Data.Nat.Base.C18 v5
        -> coe
             (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
             (du2000 (coe v0) (coe v5) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
name2014 = "Data.Nat.Properties.+-monoˡ-≤"
d2014 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2014 v0 v1 v2 v3 = du2014 v0 v3
du2014 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2014 v0 v1
  = coe (du2000 (coe v0) (coe v1) (coe (d1328 (coe v0))))
name2024 = "Data.Nat.Properties.+-monoʳ-≤"
d2024 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2024 v0 v1 v2 v3 = du2024 v0 v2 v3
du2024 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2024 v0 v1 v2
  = coe (du2000 (coe v1) (coe (d1328 (coe v0))) (coe v2))
name2030 = "Data.Nat.Properties.+-mono-<-≤"
d2030 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2030 v0 v1 v2 v3 v4 v5 = du2030 v4 v5
du2030 ::
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2030 v0 v1
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C18 v4
        -> case coe v4 of
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe
                    (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased v1
             MAlonzo.Code.Data.Nat.Base.C18 v7
               -> coe
                    (\ v8 v9 v10 -> MAlonzo.Code.Data.Nat.Base.C18 v10) erased erased
                    (du2030
                       (coe
                          (\ v8 v9 v10 -> MAlonzo.Code.Data.Nat.Base.C18 v10) erased erased
                          v7)
                       (coe v1))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name2040 = "Data.Nat.Properties.+-mono-≤-<"
d2040 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2040 v0 v1 v2 v3 v4 v5 = du2040 v3 v4 v5
du2040 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2040 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> coe (du1336 (coe v2) (coe (du1964 (coe v0))))
      MAlonzo.Code.Data.Nat.Base.C18 v5
        -> coe
             (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
             (du2040 (coe v0) (coe v5) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
name2050 = "Data.Nat.Properties.+-mono-<"
d2050 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2050 v0 v1 v2 v3 v4 = du2050 v1 v3 v4
du2050 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2050 v0 v1 v2
  = coe (du2040 (coe v1) (coe (du1442 (coe v0) (coe v2))))
name2058 = "Data.Nat.Properties.+-monoˡ-<"
d2058 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2058 v0 v1 v2 = du2058 v0
du2058 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2058 v0 = coe (du2014 (coe v0))
name2066 = "Data.Nat.Properties.+-monoʳ-<"
d2066 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2066 v0 v1 v2 v3 = du2066 v0 v3
du2066 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2066 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v3 v4 v5 -> MAlonzo.Code.Data.Nat.Base.C18 v5) erased erased
             (du2066 (coe v2) (coe v1))
name2078 = "Data.Nat.Properties.m+1+n≰m"
d2078 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d2078 = erased
name2088 = "Data.Nat.Properties.m+n≮n"
d2088 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d2088 = erased
name2102 = "Data.Nat.Properties.m+n≮m"
d2102 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d2102 = erased
name2114 = "Data.Nat.Properties.+-*-suc"
d2114 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2114 = erased
name2126 = "Data.Nat.Properties.*-identityˡ"
d2126 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2126 = erased
name2130 = "Data.Nat.Properties.*-identityʳ"
d2130 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2130 = erased
name2134 = "Data.Nat.Properties.*-identity"
d2134 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2134 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name2136 = "Data.Nat.Properties.*-zeroˡ"
d2136 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2136 = erased
name2138 = "Data.Nat.Properties.*-zeroʳ"
d2138 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2138 = erased
name2142 = "Data.Nat.Properties.*-zero"
d2142 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2142 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name2144 = "Data.Nat.Properties.*-comm"
d2144 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2144 = erased
name2154 = "Data.Nat.Properties.*-distribʳ-+"
d2154 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2154 = erased
name2168 = "Data.Nat.Properties.*-distribˡ-+"
d2168 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2168 = erased
name2170 = "Data.Nat.Properties.*-distrib-+"
d2170 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2170 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name2172 = "Data.Nat.Properties.*-assoc"
d2172 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2172 = erased
name2186 = "Data.Nat.Properties.*-isMagma"
d2186 :: MAlonzo.Code.Algebra.Structures.T84
d2186
  = coe
      (MAlonzo.Code.Algebra.Structures.C277
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         erased)
name2188 = "Data.Nat.Properties.*-isSemigroup"
d2188 :: MAlonzo.Code.Algebra.Structures.T118
d2188
  = coe (MAlonzo.Code.Algebra.Structures.C781 (coe d2186) erased)
name2190 = "Data.Nat.Properties.*-1-isMonoid"
d2190 :: MAlonzo.Code.Algebra.Structures.T268
d2190
  = coe
      (MAlonzo.Code.Algebra.Structures.C1873 (coe d2188) (coe d2134))
name2192 = "Data.Nat.Properties.*-1-isCommutativeMonoid"
d2192 :: MAlonzo.Code.Algebra.Structures.T314
d2192
  = coe
      (MAlonzo.Code.Algebra.Structures.C2409 (coe d2188) erased erased)
name2194 = "Data.Nat.Properties.*-+-isSemiring"
d2194 :: MAlonzo.Code.Algebra.Structures.T1092
d2194
  = coe
      (MAlonzo.Code.Algebra.Structures.C10649
         (coe
            (MAlonzo.Code.Algebra.Structures.C9521
               (coe d1820) (coe d2190) (coe d2170)))
         (coe d2142))
name2196 = "Data.Nat.Properties.*-+-isCommutativeSemiring"
d2196 :: MAlonzo.Code.Algebra.Structures.T1192
d2196
  = coe
      (MAlonzo.Code.Algebra.Structures.C11793
         (coe d1820) (coe d2192) erased erased)
name2198 = "Data.Nat.Properties.*-magma"
d2198 :: MAlonzo.Code.Algebra.T28
d2198
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C77 v2 v3) erased erased
      mulInt d2186
name2200 = "Data.Nat.Properties.*-semigroup"
d2200 :: MAlonzo.Code.Algebra.T76
d2200
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C285 v2 v3) erased erased
      mulInt d2188
name2202 = "Data.Nat.Properties.*-1-monoid"
d2202 :: MAlonzo.Code.Algebra.T344
d2202
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C1365 v2 v3 v4) erased
      erased mulInt (1 :: Integer) d2190
name2204 = "Data.Nat.Properties.*-1-commutativeMonoid"
d2204 :: MAlonzo.Code.Algebra.T416
d2204
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C1695 v2 v3 v4) erased
      erased mulInt (1 :: Integer) d2192
name2206 = "Data.Nat.Properties.*-+-semiring"
d2206 :: MAlonzo.Code.Algebra.T1542
d2206
  = coe
      (\ v0 v1 v2 v3 v4 v5 v6 ->
         MAlonzo.Code.Algebra.C6067 v2 v3 v4 v5 v6)
      erased erased addInt mulInt (0 :: Integer) (1 :: Integer) d2194
name2208 = "Data.Nat.Properties.*-+-commutativeSemiring"
d2208 :: MAlonzo.Code.Algebra.T1690
d2208
  = coe
      (\ v0 v1 v2 v3 v4 v5 v6 ->
         MAlonzo.Code.Algebra.C6707 v2 v3 v4 v5 v6)
      erased erased addInt mulInt (0 :: Integer) (1 :: Integer) d2196
name2216 = "Data.Nat.Properties.*-cancelʳ-≡"
d2216 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2216 = erased
name2234 = "Data.Nat.Properties.*-cancelˡ-≡"
d2234 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2234 = erased
name2248 = "Data.Nat.Properties.m*n≡0⇒m≡0∨n≡0"
d2248 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Sum.Base.T30
d2248 v0 v1 v2 = du2248 v0
du2248 :: Integer -> MAlonzo.Code.Data.Sum.Base.T30
du2248 v0
  = case coe v0 of
      0 -> coe (MAlonzo.Code.Data.Sum.Base.C38 erased)
      _ -> coe (MAlonzo.Code.Data.Sum.Base.C42 erased)
name2262 = "Data.Nat.Properties.m*n≡1⇒m≡1"
d2262 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2262 = erased
name2276 = "Data.Nat.Properties.m*n≡1⇒n≡1"
d2276 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2276 = erased
name2290 = "Data.Nat.Properties.*-cancelʳ-≤"
d2290 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2290 v0 v1 v2 v3 = du2290 v0
du2290 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du2290 v0
  = case coe v0 of
      0 -> coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 v4 -> MAlonzo.Code.Data.Nat.Base.C18 v4) erased erased
             (du2290 (coe v1))
name2300 = "Data.Nat.Properties.*-mono-≤"
d2300 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2300 v0 v1 v2 v3 v4 v5 = du2300 v1 v3 v4 v5
du2300 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2300 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> coe (\ v5 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      MAlonzo.Code.Data.Nat.Base.C18 v6
        -> let v7 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (du2000
                (coe (mulInt (coe v7) (coe v1))) (coe v3)
                (coe (du2300 (coe v7) (coe v1) (coe v6) (coe v3))))
      _ -> MAlonzo.RTE.mazUnreachableError
name2310 = "Data.Nat.Properties.*-monoˡ-≤"
d2310 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2310 v0 v1 v2 v3 = du2310 v0 v2 v3
du2310 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2310 v0 v1 v2
  = coe (du2300 (coe v1) (coe v0) (coe v2) (coe (d1328 (coe v0))))
name2320 = "Data.Nat.Properties.*-monoʳ-≤"
d2320 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2320 v0 v1 v2 v3 = du2320 v0 v2 v3
du2320 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2320 v0 v1 v2
  = coe (du2300 (coe v0) (coe v1) (coe (d1328 (coe v0))) (coe v2))
name2326 = "Data.Nat.Properties.*-mono-<"
d2326 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2326 v0 v1 v2 v3 v4 v5 = du2326 v1 v3 v4 v5
du2326 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2326 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C18 v6
        -> case coe v6 of
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe
                    (seq
                       (coe v3)
                       (coe
                          (\ v8 v9 v10 -> MAlonzo.Code.Data.Nat.Base.C18 v10) erased erased
                          (coe (\ v8 -> MAlonzo.Code.Data.Nat.Base.C10) erased)))
             MAlonzo.Code.Data.Nat.Base.C18 v9
               -> case coe v3 of
                    MAlonzo.Code.Data.Nat.Base.C18 v12
                      -> coe
                           du2050 v1
                           (mulInt (coe (subInt (coe v0) (coe (1 :: Integer)))) (coe v1))
                           (coe
                              (\ v13 v14 v15 -> MAlonzo.Code.Data.Nat.Base.C18 v15) erased erased
                              v12)
                           (du2326
                              (coe (subInt (coe v0) (coe (1 :: Integer)))) (coe v1)
                              (coe
                                 (\ v13 v14 v15 -> MAlonzo.Code.Data.Nat.Base.C18 v15) erased erased
                                 v9)
                              (coe
                                 (\ v13 v14 v15 -> MAlonzo.Code.Data.Nat.Base.C18 v15) erased erased
                                 v12))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name2338 = "Data.Nat.Properties.*-monoˡ-<"
d2338 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2338 v0 v1 v2 v3 = du2338 v0 v2 v3
du2338 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2338 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C18 v5
        -> case coe v5 of
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe
                    (\ v7 v8 v9 -> MAlonzo.Code.Data.Nat.Base.C18 v9) erased erased
                    (coe (\ v7 -> MAlonzo.Code.Data.Nat.Base.C10) erased)
             MAlonzo.Code.Data.Nat.Base.C18 v8
               -> coe
                    (du2040
                       (coe
                          (mulInt
                             (coe (subInt (coe v1) (coe (1 :: Integer))))
                             (coe (addInt (coe (1 :: Integer)) (coe v0)))))
                       (coe (d1328 (coe (addInt (coe (1 :: Integer)) (coe v0)))))
                       (coe
                          (du2338
                             (coe v0) (coe (subInt (coe v1) (coe (1 :: Integer))))
                             (coe
                                (\ v9 v10 v11 -> MAlonzo.Code.Data.Nat.Base.C18 v11) erased erased
                                v8))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name2350 = "Data.Nat.Properties.*-monoʳ-<"
d2350 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2350 v0 v1 v2 v3 = du2350 v0 v2 v3
du2350 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2350 v0 v1 v2
  = case coe v0 of
      0 -> case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C18 v5
               -> coe
                    (du2000
                       (coe (0 :: Integer))
                       (coe
                          (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased v5)
                       (coe (\ v6 -> MAlonzo.Code.Data.Nat.Base.C10) erased))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v2 of
             MAlonzo.Code.Data.Nat.Base.C18 v6
               -> coe
                    (du2000
                       (coe (mulInt (coe v0) (coe v1)))
                       (coe
                          (\ v7 v8 v9 -> MAlonzo.Code.Data.Nat.Base.C18 v9) erased erased v6)
                       (coe
                          (du1442
                             (coe (mulInt (coe v0) (coe v1)))
                             (coe
                                (du2350
                                   (coe v3) (coe v1)
                                   (coe
                                      (\ v7 v8 v9 -> MAlonzo.Code.Data.Nat.Base.C18 v9) erased
                                      erased v6))))))
             _ -> MAlonzo.RTE.mazUnreachableError
name2362 = "Data.Nat.Properties.m≤m*n"
d2362 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2362 v0 v1 v2
  = coe
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du160
         (coe d1394) (coe d1442) (coe v0) (coe (mulInt (coe v0) (coe v1)))
         (coe
            (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du228
               (coe d1394) (coe d1562)
               (coe (mulInt (coe v0) (coe (1 :: Integer))))
               (coe (mulInt (coe v0) (coe v1))) (coe (mulInt (coe v0) (coe v1)))
               (coe (du2320 (coe v0) (coe v1) (coe v2)))
               (coe
                  (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.C84
                     (coe
                        MAlonzo.Code.Relation.Binary.Core.d414
                        (MAlonzo.Code.Relation.Binary.d36 (coe d1394))
                        (mulInt (coe v0) (coe v1))))))))
name2374 = "Data.Nat.Properties.m<m*n"
d2374 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2374 v0 v1 v2 v3
  = let v4 = subInt (coe v0) (coe (1 :: Integer)) in
    coe
      (seq
         (coe v2)
         (case coe v3 of
            MAlonzo.Code.Data.Nat.Base.C18 v7
              -> let v8 = subInt (coe v1) (coe (2 :: Integer)) in
                 coe
                   (seq
                      (coe v7)
                      (coe
                         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du176
                            (coe
                               (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du202
                                  (coe d1556)
                                  (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du66)
                                  (coe d1568) (coe (du2384 (coe v4)))
                                  (coe
                                     (subInt (coe (addInt (coe v0) (coe v1))) (coe (1 :: Integer))))
                                  (coe (mulInt (coe (du2384 (coe v4))) (coe (du2386 (coe v8)))))
                                  (coe
                                     (\ v9 v10 v11 -> MAlonzo.Code.Data.Nat.Base.C18 v11) erased
                                     erased
                                     (coe
                                        (\ v9 v10 v11 -> MAlonzo.Code.Data.Nat.Base.C18 v11) erased
                                        erased (du1964 (coe v4))))
                                  (coe
                                     (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du228
                                        (coe d1394) (coe d1562)
                                        (coe (addInt (coe (du2386 (coe v8))) (coe v4)))
                                        (coe
                                           (addInt
                                              (coe (du2386 (coe v8)))
                                              (coe (mulInt (coe v4) (coe v1)))))
                                        (coe
                                           (mulInt (coe (du2384 (coe v4))) (coe (du2386 (coe v8)))))
                                        (coe
                                           (du2024
                                              (coe (du2386 (coe v8)))
                                              (coe (mulInt (coe v4) (coe v1)))
                                              (coe
                                                 (d2362
                                                    (coe v4) (coe v1)
                                                    (coe
                                                       (\ v9 v10 v11 ->
                                                          MAlonzo.Code.Data.Nat.Base.C18 v11)
                                                       erased erased
                                                       (coe
                                                          (\ v9 -> MAlonzo.Code.Data.Nat.Base.C10)
                                                          erased))))))
                                        (coe
                                           (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.C84
                                              (coe
                                                 MAlonzo.Code.Relation.Binary.Core.d414
                                                 (MAlonzo.Code.Relation.Binary.d36 (coe d1394))
                                                 (mulInt
                                                    (coe (du2384 (coe v4)))
                                                    (coe (du2386 (coe v8))))))))))))))
            _ -> MAlonzo.RTE.mazUnreachableError))
name2384 = "Data.Nat.Properties._.m"
d2384 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> Integer
d2384 v0 v1 v2 v3 = du2384 v1
du2384 :: Integer -> Integer
du2384 v0 = coe (addInt (coe (1 :: Integer)) (coe v0))
name2386 = "Data.Nat.Properties._.n"
d2386 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> Integer
d2386 v0 v1 v2 v3 = du2386 v0
du2386 :: Integer -> Integer
du2386 v0 = coe (addInt (coe (2 :: Integer)) (coe v0))
name2388 = "Data.Nat.Properties.*-cancelʳ-<"
d2388 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2388 v0 v1 v2 v3 = du2388 v0 v1 v2
du2388 ::
  Integer -> Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du2388 v0 v1 v2
  = let v3
          = let v3 = subInt (coe v1) (coe (1 :: Integer)) in
            let v4 = subInt (coe v2) (coe (1 :: Integer)) in
            coe
              (\ v5 v6 v7 -> MAlonzo.Code.Data.Nat.Base.C18 v7) erased erased
              (du2388 (coe v0) (coe v3) (coe v4)) in
    coe
      (seq
         (coe v0)
         (case coe v1 of
            0 -> case coe v2 of
                   _ | coe (geqInt (coe v2) (coe (1 :: Integer))) ->
                       coe
                         (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                         (coe (\ v4 -> MAlonzo.Code.Data.Nat.Base.C10) erased)
                   _ -> coe v3
            _ -> let v4 = subInt (coe v1) (coe (1 :: Integer)) in
                 case coe v2 of
                   _ | coe (geqInt (coe v2) (coe (1 :: Integer))) ->
                       let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                       coe
                         (\ v6 v7 v8 -> MAlonzo.Code.Data.Nat.Base.C18 v8) erased erased
                         (du2388 (coe v0) (coe v4) (coe v5))
                   _ -> coe v3))
name2404 = "Data.Nat.Properties.*-cancelˡ-<"
d2404 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2404 v0 v1 v2 = coe (\ v3 -> du2388 (coe v0) (coe v1) (coe v2))
name2420 = "Data.Nat.Properties.*-cancel-<"
d2420 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2420
  = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 (coe d2404) (coe d2388))
name2422 = "Data.Nat.Properties.^-identityʳ"
d2422 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2422 = erased
name2426 = "Data.Nat.Properties.^-zeroˡ"
d2426 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2426 = erased
name2436 = "Data.Nat.Properties.^-distribˡ-+-*"
d2436 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2436 = erased
name2454 = "Data.Nat.Properties.^-semigroup-morphism"
d2454 :: Integer -> MAlonzo.Code.Algebra.Morphism.T150
d2454 v0 = du2454
du2454 :: MAlonzo.Code.Algebra.Morphism.T150
du2454 = coe (MAlonzo.Code.Algebra.Morphism.C593 erased erased)
name2462 = "Data.Nat.Properties.^-monoid-morphism"
d2462 :: Integer -> MAlonzo.Code.Algebra.Morphism.T290
d2462 v0 = du2462
du2462 :: MAlonzo.Code.Algebra.Morphism.T290
du2462
  = coe (MAlonzo.Code.Algebra.Morphism.C881 (coe du2454) erased)
name2470 = "Data.Nat.Properties.^-*-assoc"
d2470 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2470 = erased
name2492 = "Data.Nat.Properties.m^n≡0⇒m≡0"
d2492 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2492 = erased
name2504 = "Data.Nat.Properties.m^n≡1⇒n≡0∨m≡1"
d2504 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Sum.Base.T30
d2504 v0 v1 v2 = du2504 v1
du2504 :: Integer -> MAlonzo.Code.Data.Sum.Base.T30
du2504 v0
  = case coe v0 of
      0 -> coe (MAlonzo.Code.Data.Sum.Base.C38 erased)
      _ -> coe (MAlonzo.Code.Data.Sum.Base.C42 erased)
name2514 = "Data.Nat.Properties.⊔-assoc"
d2514 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2514 = erased
name2530 = "Data.Nat.Properties.⊔-identityˡ"
d2530 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2530 = erased
name2532 = "Data.Nat.Properties.⊔-identityʳ"
d2532 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2532 = erased
name2536 = "Data.Nat.Properties.⊔-identity"
d2536 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2536 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name2538 = "Data.Nat.Properties.⊔-comm"
d2538 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2538 = erased
name2548 = "Data.Nat.Properties.⊔-sel"
d2548 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d2548 v0 v1
  = case coe v0 of
      0 -> coe (MAlonzo.Code.Data.Sum.Base.C42 erased)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (MAlonzo.Code.Data.Sum.Base.C38 erased)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (d2548 (coe v2) (coe v3))
name2572 = "Data.Nat.Properties.⊔-idem"
d2572 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2572 = erased
name2580 = "Data.Nat.Properties.⊔-least"
d2580 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2580 v0 v1 v2 v3 v4 = du2580 v0 v1 v3 v4
du2580 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2580 v0 v1 v2 v3
  = let v4 = d2548 (coe v0) (coe v1) in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C38 v5 -> coe v2
      MAlonzo.Code.Data.Sum.Base.C42 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name2622 = "Data.Nat.Properties.⊔-isMagma"
d2622 :: MAlonzo.Code.Algebra.Structures.T84
d2622
  = coe
      (MAlonzo.Code.Algebra.Structures.C277
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         erased)
name2624 = "Data.Nat.Properties.⊔-isSemigroup"
d2624 :: MAlonzo.Code.Algebra.Structures.T118
d2624
  = coe (MAlonzo.Code.Algebra.Structures.C781 (coe d2622) erased)
name2626 = "Data.Nat.Properties.⊔-isBand"
d2626 :: MAlonzo.Code.Algebra.Structures.T152
d2626
  = coe (MAlonzo.Code.Algebra.Structures.C1021 (coe d2624) erased)
name2628 = "Data.Nat.Properties.⊔-isSemilattice"
d2628 :: MAlonzo.Code.Algebra.Structures.T190
d2628
  = coe (MAlonzo.Code.Algebra.Structures.C1305 (coe d2626) erased)
name2630 = "Data.Nat.Properties.⊔-0-isCommutativeMonoid"
d2630 :: MAlonzo.Code.Algebra.Structures.T314
d2630
  = coe
      (MAlonzo.Code.Algebra.Structures.C2409 (coe d2624) erased erased)
name2632 = "Data.Nat.Properties.⊔-magma"
d2632 :: MAlonzo.Code.Algebra.T28
d2632
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C77 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d74 d2622
name2634 = "Data.Nat.Properties.⊔-semigroup"
d2634 :: MAlonzo.Code.Algebra.T76
d2634
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C285 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d74 d2624
name2636 = "Data.Nat.Properties.⊔-band"
d2636 :: MAlonzo.Code.Algebra.T132
d2636
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C515 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d74 d2626
name2638 = "Data.Nat.Properties.⊔-semilattice"
d2638 :: MAlonzo.Code.Algebra.T194
d2638
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C771 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d74 d2628
name2640 = "Data.Nat.Properties.⊔-0-commutativeMonoid"
d2640 :: MAlonzo.Code.Algebra.T416
d2640
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C1695 v2 v3 v4) erased
      erased MAlonzo.Code.Data.Nat.Base.d74 (0 :: Integer) d2630
name2648 = "Data.Nat.Properties.⊔-triangulate"
d2648 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2648 = erased
name2664 = "Data.Nat.Properties.m≤m⊔n"
d2664 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d2664 v0 v1
  = case coe v0 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (d1328 (coe v0))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (d2664 (coe v2) (coe v3))
name2676 = "Data.Nat.Properties.n≤m⊔n"
d2676 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d2676 v0 v1 = coe (d2664 (coe v1) (coe v0))
name2688 = "Data.Nat.Properties.m≤n⇒n⊔m≡n"
d2688 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2688 = erased
name2696 = "Data.Nat.Properties.m≤n⇒m⊔n≡n"
d2696 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d2696 = erased
name2706 = "Data.Nat.Properties.n⊔m≡m⇒n≤m"
d2706 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d2706 v0 v1 v2 = du2706 v0 v1
du2706 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du2706 v0 v1 = coe (d2664 (coe v1) (coe v0))
name2716 = "Data.Nat.Properties.n⊔m≡n⇒m≤n"
d2716 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d2716 v0 v1 v2 = du2716 v0 v1
du2716 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du2716 v0 v1 = coe (d2676 (coe v1) (coe v0))
name2728 = "Data.Nat.Properties.m≤n⇒m≤n⊔o"
d2728 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2728 v0 v1 v2 v3 = du2728 v1 v2 v3
du2728 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2728 v0 v1 v2
  = coe (du1336 (coe v2) (coe (d2664 (coe v0) (coe v1))))
name2740 = "Data.Nat.Properties.m≤n⇒m≤o⊔n"
d2740 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2740 v0 v1 v2 v3 = du2740 v1 v2 v3
du2740 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2740 v0 v1 v2
  = coe (du1336 (coe v2) (coe (d2676 (coe v1) (coe v0))))
name2752 = "Data.Nat.Properties.m⊔n≤o⇒m≤o"
d2752 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2752 v0 v1 v2 v3 = du2752 v0 v1 v3
du2752 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2752 v0 v1 v2
  = coe (du1336 (coe (d2664 (coe v0) (coe v1))) (coe v2))
name2766 = "Data.Nat.Properties.m⊔n≤o⇒n≤o"
d2766 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2766 v0 v1 v2 v3 = du2766 v0 v1 v3
du2766 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2766 v0 v1 v2
  = coe (du1336 (coe (d2676 (coe v0) (coe v1))) (coe v2))
name2780 = "Data.Nat.Properties.m<n⇒m<n⊔o"
d2780 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2780 v0 v1 = du2780 v1
du2780 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2780 v0 = coe (du2728 (coe v0))
name2788 = "Data.Nat.Properties.m<n⇒m<o⊔n"
d2788 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2788 v0 v1 = du2788 v1
du2788 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2788 v0 = coe (du2740 (coe v0))
name2796 = "Data.Nat.Properties.m⊔n<o⇒m<o"
d2796 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2796 v0 v1 v2 v3 = du2796 v0 v1 v3
du2796 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2796 v0 v1 v2
  = coe (du1562 (coe (d2664 (coe v0) (coe v1))) (coe v2))
name2810 = "Data.Nat.Properties.m⊔n<o⇒n<o"
d2810 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2810 v0 v1 v2 v3 = du2810 v0 v1 v3
du2810 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du2810 v0 v1 v2
  = coe (du1562 (coe (d2676 (coe v0) (coe v1))) (coe v2))
name2818 = "Data.Nat.Properties.⊔-mono-≤"
d2818 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2818 v0 v1 v2 v3 v4 v5
  = let v6 = d2548 (coe v0) (coe v2) in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C38 v7
        -> coe (du1336 (coe v4) (coe (d2664 (coe v1) (coe v3))))
      MAlonzo.Code.Data.Sum.Base.C42 v7
        -> coe (du1336 (coe v5) (coe (d2676 (coe v1) (coe v3))))
      _ -> MAlonzo.RTE.mazUnreachableError
name2876 = "Data.Nat.Properties.⊔-monoˡ-≤"
d2876 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2876 v0 v1 v2 v3
  = coe
      (d2818
         (coe v1) (coe v2) (coe v0) (coe v0) (coe v3)
         (coe (d1328 (coe v0))))
name2886 = "Data.Nat.Properties.⊔-monoʳ-≤"
d2886 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2886 v0 v1 v2 v3
  = coe
      (d2818
         (coe v0) (coe v0) (coe v1) (coe v2) (coe (d1328 (coe v0)))
         (coe v3))
name2892 = "Data.Nat.Properties.⊔-mono-<"
d2892 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d2892 v0 v1 v2 v3
  = coe
      (d2818
         (coe (addInt (coe (1 :: Integer)) (coe v0))) (coe v1)
         (coe (addInt (coe (1 :: Integer)) (coe v2))) (coe v3))
name2894 = "Data.Nat.Properties.+-distribˡ-⊔"
d2894 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2894 = erased
name2906 = "Data.Nat.Properties.+-distribʳ-⊔"
d2906 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2906 = erased
name2908 = "Data.Nat.Properties.+-distrib-⊔"
d2908 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2908 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name2914 = "Data.Nat.Properties.m⊔n≤m+n"
d2914 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d2914 v0 v1
  = let v2 = d2548 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C38 v3 -> coe (du1952 (coe v0))
      MAlonzo.Code.Data.Sum.Base.C42 v3 -> coe (du1964 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name2944 = "Data.Nat.Properties.⊓-assoc"
d2944 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2944 = erased
name2960 = "Data.Nat.Properties.⊓-zeroˡ"
d2960 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2960 = erased
name2962 = "Data.Nat.Properties.⊓-zeroʳ"
d2962 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2962 = erased
name2966 = "Data.Nat.Properties.⊓-zero"
d2966 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d2966 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name2968 = "Data.Nat.Properties.⊓-comm"
d2968 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d2968 = erased
name2978 = "Data.Nat.Properties.⊓-sel"
d2978 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d2978 v0 v1
  = case coe v0 of
      0 -> coe (MAlonzo.Code.Data.Sum.Base.C38 erased)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (MAlonzo.Code.Data.Sum.Base.C42 erased)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (d2978 (coe v2) (coe v3))
name3002 = "Data.Nat.Properties.⊓-idem"
d3002 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3002 = erased
name3010 = "Data.Nat.Properties.⊓-greatest"
d3010 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3010 v0 v1 v2 v3 v4 = du3010 v0 v1 v3 v4
du3010 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3010 v0 v1 v2 v3
  = let v4 = d2978 (coe v0) (coe v1) in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C38 v5 -> coe v2
      MAlonzo.Code.Data.Sum.Base.C42 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name3052 = "Data.Nat.Properties.⊓-distribʳ-⊔"
d3052 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3052 = erased
name3072 = "Data.Nat.Properties.⊓-distribˡ-⊔"
d3072 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3072 = erased
name3074 = "Data.Nat.Properties.⊓-distrib-⊔"
d3074 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d3074 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name3076 = "Data.Nat.Properties.⊔-abs-⊓"
d3076 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3076 = erased
name3086 = "Data.Nat.Properties.⊓-abs-⊔"
d3086 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3086 = erased
name3098 = "Data.Nat.Properties.⊓-⊔-absorptive"
d3098 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d3098 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name3100 = "Data.Nat.Properties.⊓-isMagma"
d3100 :: MAlonzo.Code.Algebra.Structures.T84
d3100
  = coe
      (MAlonzo.Code.Algebra.Structures.C277
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         erased)
name3102 = "Data.Nat.Properties.⊓-isSemigroup"
d3102 :: MAlonzo.Code.Algebra.Structures.T118
d3102
  = coe (MAlonzo.Code.Algebra.Structures.C781 (coe d3100) erased)
name3104 = "Data.Nat.Properties.⊓-isBand"
d3104 :: MAlonzo.Code.Algebra.Structures.T152
d3104
  = coe (MAlonzo.Code.Algebra.Structures.C1021 (coe d3102) erased)
name3106 = "Data.Nat.Properties.⊓-isSemilattice"
d3106 :: MAlonzo.Code.Algebra.Structures.T190
d3106
  = coe (MAlonzo.Code.Algebra.Structures.C1305 (coe d3104) erased)
name3108 = "Data.Nat.Properties.⊔-⊓-isSemiringWithoutOne"
d3108 :: MAlonzo.Code.Algebra.Structures.T826
d3108
  = coe
      (MAlonzo.Code.Algebra.Structures.C7939
         (coe d2630) (coe d3102) (coe d3074) (coe d2966))
name3110
  = "Data.Nat.Properties.⊔-⊓-isCommutativeSemiringWithoutOne"
d3110 :: MAlonzo.Code.Algebra.Structures.T914
d3110
  = coe (MAlonzo.Code.Algebra.Structures.C8849 (coe d3108) erased)
name3112 = "Data.Nat.Properties.⊓-⊔-isLattice"
d3112 :: MAlonzo.Code.Algebra.Structures.T620
d3112
  = coe
      (MAlonzo.Code.Algebra.Structures.C5667
         (coe MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du70)
         erased erased erased erased erased erased (coe d3098))
name3114 = "Data.Nat.Properties.⊓-⊔-isDistributiveLattice"
d3114 :: MAlonzo.Code.Algebra.Structures.T692
d3114
  = coe (MAlonzo.Code.Algebra.Structures.C6793 (coe d3112) erased)
name3116 = "Data.Nat.Properties.⊓-magma"
d3116 :: MAlonzo.Code.Algebra.T28
d3116
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C77 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d84 d3100
name3118 = "Data.Nat.Properties.⊓-semigroup"
d3118 :: MAlonzo.Code.Algebra.T76
d3118
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C285 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d74 d2624
name3120 = "Data.Nat.Properties.⊓-band"
d3120 :: MAlonzo.Code.Algebra.T132
d3120
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C515 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d84 d3104
name3122 = "Data.Nat.Properties.⊓-semilattice"
d3122 :: MAlonzo.Code.Algebra.T194
d3122
  = coe
      (\ v0 v1 v2 v3 -> MAlonzo.Code.Algebra.C771 v2 v3) erased erased
      MAlonzo.Code.Data.Nat.Base.d84 d3106
name3124 = "Data.Nat.Properties.⊔-⊓-commutativeSemiringWithoutOne"
d3124 :: MAlonzo.Code.Algebra.T1254
d3124
  = coe
      (\ v0 v1 v2 v3 v4 v5 -> MAlonzo.Code.Algebra.C4977 v2 v3 v4 v5)
      erased erased MAlonzo.Code.Data.Nat.Base.d74
      MAlonzo.Code.Data.Nat.Base.d84 (0 :: Integer) d3110
name3126 = "Data.Nat.Properties.⊓-⊔-lattice"
d3126 :: MAlonzo.Code.Algebra.T880
d3126
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C3441 v2 v3 v4) erased
      erased MAlonzo.Code.Data.Nat.Base.d84
      MAlonzo.Code.Data.Nat.Base.d74 d3112
name3128 = "Data.Nat.Properties.⊓-⊔-distributiveLattice"
d3128 :: MAlonzo.Code.Algebra.T950
d3128
  = coe
      (\ v0 v1 v2 v3 v4 -> MAlonzo.Code.Algebra.C3739 v2 v3 v4) erased
      erased MAlonzo.Code.Data.Nat.Base.d84
      MAlonzo.Code.Data.Nat.Base.d74 d3114
name3136 = "Data.Nat.Properties.⊓-triangulate"
d3136 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3136 = erased
name3152 = "Data.Nat.Properties.m⊓n≤m"
d3152 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3152 v0 v1
  = case coe v0 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (\ v3 -> MAlonzo.Code.Data.Nat.Base.C10) erased
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (d3152 (coe v2) (coe v3))
name3164 = "Data.Nat.Properties.m⊓n≤n"
d3164 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3164 v0 v1 = coe (d3152 (coe v1) (coe v0))
name3176 = "Data.Nat.Properties.m≤n⇒m⊓n≡m"
d3176 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3176 = erased
name3184 = "Data.Nat.Properties.m≤n⇒n⊓m≡m"
d3184 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3184 = erased
name3194 = "Data.Nat.Properties.m⊓n≡m⇒m≤n"
d3194 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d3194 v0 v1 v2 = du3194 v0 v1
du3194 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du3194 v0 v1 = coe (d3164 (coe v0) (coe v1))
name3204 = "Data.Nat.Properties.m⊓n≡n⇒n≤m"
d3204 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d3204 v0 v1 v2 = du3204 v0 v1
du3204 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du3204 v0 v1 = coe (d3152 (coe v0) (coe v1))
name3216 = "Data.Nat.Properties.m≤n⇒m⊓o≤n"
d3216 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3216 v0 v1 v2 v3 = du3216 v0 v2 v3
du3216 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3216 v0 v1 v2
  = coe (du1336 (coe (d3152 (coe v0) (coe v1))) (coe v2))
name3228 = "Data.Nat.Properties.m≤n⇒o⊓m≤n"
d3228 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3228 v0 v1 v2 v3 = du3228 v0 v2 v3
du3228 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3228 v0 v1 v2
  = coe (du1336 (coe (d3164 (coe v1) (coe v0))) (coe v2))
name3240 = "Data.Nat.Properties.m≤n⊓o⇒m≤n"
d3240 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3240 v0 v1 v2 v3 = du3240 v1 v2 v3
du3240 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3240 v0 v1 v2
  = coe (du1336 (coe v2) (coe (d3152 (coe v0) (coe v1))))
name3254 = "Data.Nat.Properties.m≤n⊓o⇒m≤o"
d3254 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3254 v0 v1 v2 v3 = du3254 v1 v2 v3
du3254 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3254 v0 v1 v2
  = coe (du1336 (coe v2) (coe (d3164 (coe v0) (coe v1))))
name3268 = "Data.Nat.Properties.m<n⇒m⊓o<n"
d3268 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3268 v0 v1 v2 v3 = du3268 v0 v2 v3
du3268 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3268 v0 v1 v2
  = coe (du1562 (coe (d3152 (coe v0) (coe v1))) (coe v2))
name3280 = "Data.Nat.Properties.m<n⇒o⊓m<n"
d3280 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3280 v0 v1 v2 v3 = du3280 v0 v2 v3
du3280 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3280 v0 v1 v2
  = coe (du1562 (coe (d3164 (coe v1) (coe v0))) (coe v2))
name3292 = "Data.Nat.Properties.m<n⊓o⇒m<n"
d3292 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3292 v0 = du3292
du3292 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3292 = coe du3240
name3300 = "Data.Nat.Properties.m<n⊓o⇒m<o"
d3300 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3300 v0 = du3300
du3300 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du3300 = coe du3254
name3302 = "Data.Nat.Properties.⊓-mono-≤"
d3302 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3302 v0 v1 v2 v3 v4 v5
  = let v6 = d2978 (coe v1) (coe v3) in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C38 v7
        -> coe (du1336 (coe (d3152 (coe v0) (coe v2))) (coe v4))
      MAlonzo.Code.Data.Sum.Base.C42 v7
        -> coe (du1336 (coe (d3164 (coe v0) (coe v2))) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
name3360 = "Data.Nat.Properties.⊓-monoˡ-≤"
d3360 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3360 v0 v1 v2 v3
  = coe
      (d3302
         (coe v1) (coe v2) (coe v0) (coe v0) (coe v3)
         (coe (d1328 (coe v0))))
name3370 = "Data.Nat.Properties.⊓-monoʳ-≤"
d3370 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3370 v0 v1 v2 v3
  = coe
      (d3302
         (coe v0) (coe v0) (coe v1) (coe v2) (coe (d1328 (coe v0)))
         (coe v3))
name3376 = "Data.Nat.Properties.⊓-mono-<"
d3376 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3376 v0 v1 v2 v3
  = coe
      (d3302
         (coe (addInt (coe (1 :: Integer)) (coe v0))) (coe v1)
         (coe (addInt (coe (1 :: Integer)) (coe v2))) (coe v3))
name3382 = "Data.Nat.Properties.m⊓n≤m⊔n"
d3382 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3382 v0 v1
  = case coe v0 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (\ v3 -> MAlonzo.Code.Data.Nat.Base.C10) erased
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (d3382 (coe v2) (coe v3))
name3392 = "Data.Nat.Properties.+-distribˡ-⊓"
d3392 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3392 = erased
name3404 = "Data.Nat.Properties.+-distribʳ-⊓"
d3404 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3404 = erased
name3406 = "Data.Nat.Properties.+-distrib-⊓"
d3406 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d3406 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name3412 = "Data.Nat.Properties.m⊓n≤m+n"
d3412 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3412 v0 v1
  = let v2 = d2978 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C38 v3 -> coe (du1952 (coe v0))
      MAlonzo.Code.Data.Sum.Base.C42 v3 -> coe (du1964 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name3442 = "Data.Nat.Properties.0∸n≡0"
d3442 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3442 = erased
name3446 = "Data.Nat.Properties.n∸n≡0"
d3446 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3446 = erased
name3454 = "Data.Nat.Properties.n∸m≤n"
d3454 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3454 v0 v1
  = case coe v0 of
      0 -> coe
             (d1328 (coe MAlonzo.Code.Agda.Builtin.Nat.d22 v1 (0 :: Integer)))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    (d1328 (coe MAlonzo.Code.Agda.Builtin.Nat.d22 (0 :: Integer) v0))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (du1336 (coe (d3454 (coe v2) (coe v3))) (coe (d1430 (coe v3))))
name3468 = "Data.Nat.Properties.m≮m∸n"
d3468 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d3468 = erased
name3476 = "Data.Nat.Properties.∸-mono"
d3476 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3476 v0 v1 v2 v3 v4 v5
  = let v6
          = seq
              (coe v5) (coe (du1336 (coe (d3454 (coe v2) (coe v0))) (coe v4))) in
    case coe v4 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> case coe v5 of
             MAlonzo.Code.Data.Nat.Base.C18 v10
               -> case coe v2 of
                    _ | coe (geqInt (coe v2) (coe (1 :: Integer))) ->
                        case coe v3 of
                          _ | coe (geqInt (coe v3) (coe (1 :: Integer))) ->
                              coe (\ v11 -> MAlonzo.Code.Data.Nat.Base.C10) erased
                          _ -> coe v6
                    _ -> coe v6
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe
                    (du1336
                       (coe (d3454 (coe v2) (coe v0)))
                       (coe (\ v9 -> MAlonzo.Code.Data.Nat.Base.C10) erased))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Nat.Base.C18 v9
        -> case coe v5 of
             MAlonzo.Code.Data.Nat.Base.C18 v12
               -> case coe v0 of
                    _ | coe (geqInt (coe v0) (coe (1 :: Integer))) ->
                        let v13 = subInt (coe v0) (coe (1 :: Integer)) in
                        case coe v1 of
                          _ | coe (geqInt (coe v1) (coe (1 :: Integer))) ->
                              let v14 = subInt (coe v1) (coe (1 :: Integer)) in
                              case coe v2 of
                                _ | coe (geqInt (coe v2) (coe (1 :: Integer))) ->
                                    let v15 = subInt (coe v2) (coe (1 :: Integer)) in
                                    case coe v3 of
                                      _ | coe (geqInt (coe v3) (coe (1 :: Integer))) ->
                                          let v16 = subInt (coe v3) (coe (1 :: Integer)) in
                                          coe
                                            (d3476
                                               (coe v13) (coe v14) (coe v15) (coe v16) (coe v9)
                                               (coe v12))
                                      _ -> coe v6
                                _ -> coe v6
                          _ -> coe v6
                    _ -> coe v6
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe
                    (du1336
                       (coe (d3454 (coe v2) (coe v0)))
                       (coe
                          (\ v11 v12 v13 -> MAlonzo.Code.Data.Nat.Base.C18 v13) erased erased
                          v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name3494 = "Data.Nat.Properties.∸-monoˡ-≤"
d3494 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3494 v0 v1 v2 v3
  = coe
      (d3476
         (coe v0) (coe v1) (coe v2) (coe v2) (coe v3)
         (coe (d1328 (coe v2))))
name3506 = "Data.Nat.Properties.∸-monoʳ-≤"
d3506 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d3506 v0 v1 v2 v3
  = coe
      (d3476
         (coe v2) (coe v2) (coe v1) (coe v0) (coe (d1328 (coe v2)))
         (coe v3))
name3514 = "Data.Nat.Properties.m∸n≡0⇒m≤n"
d3514 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d3514 v0 v1 v2 = du3514 v0
du3514 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du3514 v0
  = case coe v0 of
      0 -> coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 v4 -> MAlonzo.Code.Data.Nat.Base.C18 v4) erased erased
             (du3514 (coe v1))
name3526 = "Data.Nat.Properties.m≤n⇒m∸n≡0"
d3526 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3526 = erased
name3536 = "Data.Nat.Properties.m∸n≢0⇒n<m"
d3536 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T12 ->
   MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d3536 v0 v1 v2 = du3536 v0 v1
du3536 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du3536 v0 v1
  = let v2 = d1606 (coe v1) (coe v0) in
    case coe v2 of
      MAlonzo.Code.Relation.Nullary.C22 v3 -> coe v3
      MAlonzo.Code.Relation.Nullary.C26
        -> coe MAlonzo.Code.Data.Empty.d10 () erased erased
      _ -> MAlonzo.RTE.mazUnreachableError
name3570 = "Data.Nat.Properties.+-∸-comm"
d3570 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3570 = erased
name3588 = "Data.Nat.Properties.∸-+-assoc"
d3588 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3588 = erased
name3618 = "Data.Nat.Properties.+-∸-assoc"
d3618 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3618 = erased
name3638 = "Data.Nat.Properties.m≤n+m∸n"
d3638 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3638 v0 v1
  = case coe v0 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (d1328 (coe v0))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (d3638 (coe v2) (coe v3))
name3652 = "Data.Nat.Properties.m+n∸n≡m"
d3652 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3652 = erased
name3664 = "Data.Nat.Properties.m+n∸m≡n"
d3664 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3664 = erased
name3676 = "Data.Nat.Properties.m+[n∸m]≡n"
d3676 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3676 = erased
name3690 = "Data.Nat.Properties.m∸n+n≡m"
d3690 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3690 = erased
name3702 = "Data.Nat.Properties.m∸[m∸n]≡n"
d3702 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3702 = erased
name3718 = "Data.Nat.Properties.[m+n]∸[m+o]≡n∸o"
d3718 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3718 = erased
name3730 = "Data.Nat.Properties.*-distribʳ-∸"
d3730 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3730 = erased
name3750 = "Data.Nat.Properties.*-distribˡ-∸"
d3750 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3750 = erased
name3752 = "Data.Nat.Properties.*-distrib-∸"
d3752 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d3752 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name3758 = "Data.Nat.Properties.m⊓n+n∸m≡n"
d3758 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3758 = erased
name3772 = "Data.Nat.Properties.[m∸n]⊓[n∸m]≡0"
d3772 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3772 = erased
name3788 = "Data.Nat.Properties.∸-distribˡ-⊓-⊔"
d3788 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3788 = erased
name3814 = "Data.Nat.Properties.∸-distribʳ-⊓"
d3814 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3814 = erased
name3840 = "Data.Nat.Properties.∸-distribˡ-⊔-⊓"
d3840 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3840 = erased
name3864 = "Data.Nat.Properties.∸-distribʳ-⊔"
d3864 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3864 = erased
name3888 = "Data.Nat.Properties.m≡n⇒∣m-n∣≡0"
d3888 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3888 = erased
name3896 = "Data.Nat.Properties.∣m-n∣≡0⇒m≡n"
d3896 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3896 = erased
name3910 = "Data.Nat.Properties.m≤n⇒∣n-m∣≡n∸m"
d3910 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d3910 = erased
name3920 = "Data.Nat.Properties.∣m-n∣≡m∸n⇒n≤m"
d3920 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d3920 v0 v1 v2 = du3920 v0 v1
du3920 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
du3920 v0 v1
  = case coe v0 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (\ v3 -> MAlonzo.Code.Data.Nat.Base.C10) erased
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (du3920 (coe v2) (coe v3))
name3936 = "Data.Nat.Properties.∣n-n∣≡0"
d3936 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3936 = erased
name3944 = "Data.Nat.Properties.∣m-m+n∣≡n"
d3944 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3944 = erased
name3958 = "Data.Nat.Properties.∣m+n-m+o∣≡∣n-o|"
d3958 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d3958 = erased
name3974 = "Data.Nat.Properties.m∸n≤∣m-n∣"
d3974 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d3974 v0 v1
  = let v2 = d1342 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C38 v3
        -> coe (\ v4 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      MAlonzo.Code.Data.Sum.Base.C42 v3
        -> coe (d1328 (coe MAlonzo.Code.Agda.Builtin.Nat.d22 v0 v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name4004 = "Data.Nat.Properties.∣m-n∣≤m⊔n"
d4004 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d4004 v0 v1
  = case coe v0 of
      0 -> coe
             (d1328
                (coe
                   (MAlonzo.Code.Data.Nat.Base.d110 (coe (0 :: Integer)) (coe v1))))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe
                    (d1328
                       (coe
                          (MAlonzo.Code.Data.Nat.Base.d110 (coe v0) (coe (0 :: Integer)))))
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe (d4004 (coe v2) (coe v3))
name4014 = "Data.Nat.Properties.∣-∣-comm"
d4014 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4014 = erased
name4028 = "Data.Nat.Properties.∣m-n∣≡[m∸n]∨[n∸m]"
d4028 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d4028 v0 v1
  = let v2 = d1342 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C38 v3
        -> coe
             (MAlonzo.Code.Data.Sum.Base.C42
                (coe
                   (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du190
                      (coe
                         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.C84
                            (coe
                               MAlonzo.Code.Relation.Binary.Core.d414
                               (MAlonzo.Code.Relation.Binary.d36 (coe d1394))
                               (coe MAlonzo.Code.Agda.Builtin.Nat.d22 v1 v0)))))))
      MAlonzo.Code.Data.Sum.Base.C42 v3
        -> coe (MAlonzo.Code.Data.Sum.Base.C38 erased)
      _ -> MAlonzo.RTE.mazUnreachableError
name4056 = "Data.Nat.Properties.*-distribˡ-∣-∣-aux"
d4056 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4056 = erased
name4068 = "Data.Nat.Properties.*-distribˡ-∣-∣"
d4068 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4068 = erased
name4098 = "Data.Nat.Properties.*-distribʳ-∣-∣"
d4098 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4098 = erased
name4100 = "Data.Nat.Properties.*-distrib-∣-∣"
d4100 :: MAlonzo.Code.Agda.Builtin.Sigma.T14
d4100 = coe (MAlonzo.Code.Agda.Builtin.Sigma.C32 erased erased)
name4102 = "Data.Nat.Properties.⌊n/2⌋-mono"
d4102 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4102 v0 v1 v2 = du4102 v2
du4102 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du4102 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C10
        -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      MAlonzo.Code.Data.Nat.Base.C18 v3
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Base.C10
               -> coe (\ v5 -> MAlonzo.Code.Data.Nat.Base.C10) erased
             MAlonzo.Code.Data.Nat.Base.C18 v6
               -> coe
                    (\ v7 v8 v9 -> MAlonzo.Code.Data.Nat.Base.C18 v9) erased erased
                    (du4102 (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
name4106 = "Data.Nat.Properties.⌈n/2⌉-mono"
d4106 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4106 v0 v1 v2 = du4106 v2
du4106 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du4106 v0
  = coe
      (du4102
         (coe
            (\ v1 v2 v3 -> MAlonzo.Code.Data.Nat.Base.C18 v3) erased erased
            v0))
name4110 = "Data.Nat.Properties.≤′-trans"
d4110 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T122 ->
  MAlonzo.Code.Data.Nat.Base.T122 -> MAlonzo.Code.Data.Nat.Base.T122
d4110 v0 v1 v2 v3 v4 = du4110 v3 v4
du4110 ::
  MAlonzo.Code.Data.Nat.Base.T122 ->
  MAlonzo.Code.Data.Nat.Base.T122 -> MAlonzo.Code.Data.Nat.Base.T122
du4110 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C126 -> coe v0
      MAlonzo.Code.Data.Nat.Base.C132 v3
        -> coe
             (\ v4 v5 -> MAlonzo.Code.Data.Nat.Base.C132 v5) erased
             (du4110 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
name4120 = "Data.Nat.Properties.z≤′n"
d4120 :: Integer -> MAlonzo.Code.Data.Nat.Base.T122
d4120 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C126
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C132 v3) erased
             (d4120 (coe v1))
name4128 = "Data.Nat.Properties.s≤′s"
d4128 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T122 -> MAlonzo.Code.Data.Nat.Base.T122
d4128 v0 v1 v2 = du4128 v2
du4128 ::
  MAlonzo.Code.Data.Nat.Base.T122 -> MAlonzo.Code.Data.Nat.Base.T122
du4128 v0 = coe v0
name4132 = "Data.Nat.Properties.≤′⇒≤"
d4132 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T122 -> MAlonzo.Code.Data.Nat.Base.T6
d4132 v0 v1 v2 = du4132 v0 v2
du4132 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T122 -> MAlonzo.Code.Data.Nat.Base.T6
du4132 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C126 -> coe (d1328 (coe v0))
      MAlonzo.Code.Data.Nat.Base.C132 v3
        -> coe (du4132 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
name4136 = "Data.Nat.Properties.≤⇒≤′"
d4136 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T122
d4136 v0 v1 v2 = du4136 v1 v2
du4136 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T122
du4136 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Nat.Base.C10 -> coe (d4120 (coe v0))
      MAlonzo.Code.Data.Nat.Base.C18 v4
        -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (du4136 (coe v5) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
name4148 = "Data.Nat.Properties.≤′-step-injective"
d4148 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T122 ->
  MAlonzo.Code.Data.Nat.Base.T122 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4148 = erased
name4150 = "Data.Nat.Properties._≤′?_"
d4150 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4150 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du24 (du4136 (coe v1))
      (du4132 (coe v0)) (d1370 (coe v0) (coe v1))
name4156 = "Data.Nat.Properties._<′?_"
d4156 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4156 v0 v1
  = coe (d4150 (coe (addInt (coe (1 :: Integer)) (coe v0))) (coe v1))
name4162 = "Data.Nat.Properties._≥′?_"
d4162 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4162 v0 v1 = coe (d4150 (coe v1) (coe v0))
name4164 = "Data.Nat.Properties._>′?_"
d4164 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4164 v0 v1 = coe (d4156 (coe v1) (coe v0))
name4170 = "Data.Nat.Properties.m≤′m+n"
d4170 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T122
d4170 v0 v1
  = coe
      (du4136 (coe (addInt (coe v0) (coe v1))) (coe (du1952 (coe v0))))
name4180 = "Data.Nat.Properties.n≤′m+n"
d4180 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T122
d4180 v0 v1 = du4180 v0
du4180 :: Integer -> MAlonzo.Code.Data.Nat.Base.T122
du4180 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C126
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C132 v3) erased
             (du4180 (coe v1))
name4190 = "Data.Nat.Properties.⌈n/2⌉≤′n"
d4190 :: Integer -> MAlonzo.Code.Data.Nat.Base.T122
d4190 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C126
      1 -> coe MAlonzo.Code.Data.Nat.Base.C126
      _ -> let v1 = subInt (coe v0) (coe (2 :: Integer)) in
           coe
             (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C132 v3) erased
             (d4190 (coe v1))
name4196 = "Data.Nat.Properties.⌊n/2⌋≤′n"
d4196 :: Integer -> MAlonzo.Code.Data.Nat.Base.T122
d4196 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Nat.Base.C126
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C132 v3) erased
             (d4190 (coe v1))
name4204 = "Data.Nat.Properties.m<ᵇn⇒1+m+[n-1+m]≡n"
d4204 ::
  Integer ->
  Integer -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4204 = erased
name4216 = "Data.Nat.Properties.m<ᵇ1+m+n"
d4216 :: Integer -> Integer -> AgdaAny
d4216 = erased
name4224 = "Data.Nat.Properties.<ᵇ⇒<″"
d4224 ::
  Integer -> Integer -> AgdaAny -> MAlonzo.Code.Data.Nat.Base.T156
d4224 v0 v1 v2 = du4224 v0 v1
du4224 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T156
du4224 v0 v1
  = coe
      (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C170 v2)
      (coe
         MAlonzo.Code.Agda.Builtin.Nat.d22 v1
         (addInt (coe (1 :: Integer)) (coe v0)))
      erased
name4236 = "Data.Nat.Properties.<″⇒<ᵇ"
d4236 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T156 -> AgdaAny
d4236 = erased
name4240 = "Data.Nat.Properties.≤″⇒≤"
d4240 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 -> MAlonzo.Code.Data.Nat.Base.T6
d4240 v0 v1 v2 = du4240 v0 v2
du4240 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 -> MAlonzo.Code.Data.Nat.Base.T6
du4240 v0 v1
  = case coe v0 of
      0 -> coe
             (seq
                (coe v1) (coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C170 v3
               -> coe
                    (\ v5 v6 v7 -> MAlonzo.Code.Data.Nat.Base.C18 v7) erased erased
                    (du4240
                       (coe v2)
                       (coe (\ v5 v6 -> MAlonzo.Code.Data.Nat.Base.C170 v5) v3 erased))
             _ -> MAlonzo.RTE.mazUnreachableError
name4244 = "Data.Nat.Properties.≤⇒≤″"
d4244 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T156
d4244 v0 v1 v2 = du4244 v0 v1
du4244 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T156
du4244 v0 v1
  = coe
      (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C170 v2)
      (coe MAlonzo.Code.Agda.Builtin.Nat.d22 v1 v0) erased
name4246 = "Data.Nat.Properties._<″?_"
d4246 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4246 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du24
      (\ v2 -> du4224 (coe v0) (coe v1)) erased
      (MAlonzo.Code.Data.Bool.Properties.d1928
         (coe (ltInt (coe v0) (coe v1))))
name4252 = "Data.Nat.Properties._≤″?_"
d4252 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4252 v0 v1
  = case coe v0 of
      0 -> coe
             (MAlonzo.Code.Relation.Nullary.C22
                (coe (\ v2 v3 -> MAlonzo.Code.Data.Nat.Base.C170 v2) v1 erased))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe (d4246 (coe v2) (coe v1))
name4260 = "Data.Nat.Properties._≥″?_"
d4260 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4260 v0 v1 = coe (d4252 (coe v1) (coe v0))
name4262 = "Data.Nat.Properties._>″?_"
d4262 :: Integer -> Integer -> MAlonzo.Code.Relation.Nullary.T14
d4262 v0 v1 = coe (d4246 (coe v1) (coe v0))
name4264 = "Data.Nat.Properties.≤″-irrelevant"
d4264 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4264 = erased
name4282 = "Data.Nat.Properties.<″-irrelevant"
d4282 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4282 = erased
name4284 = "Data.Nat.Properties.>″-irrelevant"
d4284 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4284 = erased
name4286 = "Data.Nat.Properties.≥″-irrelevant"
d4286 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Data.Nat.Base.T156 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4286 = erased
name4292 = "Data.Nat.Properties.≤‴⇒≤″"
d4292 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T190 -> MAlonzo.Code.Data.Nat.Base.T156
d4292 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Nat.Base.C194
        -> coe
             (\ v4 v5 -> MAlonzo.Code.Data.Nat.Base.C170 v4) (0 :: Integer)
             erased
      MAlonzo.Code.Data.Nat.Base.C200 v5
        -> coe
             (\ v6 v7 -> MAlonzo.Code.Data.Nat.Base.C170 v6)
             (addInt
                (coe (1 :: Integer))
                (coe
                   (MAlonzo.Code.Data.Nat.Base.d166
                      (coe (d4304 (coe v0) (coe v1) (coe v5))))))
             erased
      _ -> MAlonzo.RTE.mazUnreachableError
name4304 = "Data.Nat.Properties._.ind"
d4304 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T190 -> MAlonzo.Code.Data.Nat.Base.T156
d4304 v0 v1 v2
  = coe
      (d4292
         (coe (addInt (coe (1 :: Integer)) (coe v0))) (coe v1) (coe v2))
name4312 = "Data.Nat.Properties.m≤‴m+k"
d4312 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T190
d4312 v0 v1 v2 v3 = du4312 v2
du4312 :: Integer -> MAlonzo.Code.Data.Nat.Base.T190
du4312 v0
  = case coe v0 of
      0 -> coe (\ v1 -> MAlonzo.Code.Data.Nat.Base.C194) erased
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (\ v2 v3 v4 -> MAlonzo.Code.Data.Nat.Base.C200 v4) erased erased
             (du4312 (coe v1))
name4328 = "Data.Nat.Properties.≤″⇒≤‴"
d4328 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T156 -> MAlonzo.Code.Data.Nat.Base.T190
d4328 v0 v1 v2 = du4328 v2
du4328 ::
  MAlonzo.Code.Data.Nat.Base.T156 -> MAlonzo.Code.Data.Nat.Base.T190
du4328 v0
  = case coe v0 of
      MAlonzo.Code.Data.Nat.Base.C170 v1 -> coe (du4312 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
name4338 = "Data.Nat.Properties.eq?"
d4338 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Function.Injection.T88 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Relation.Nullary.T14
d4338 v0 v1 v2 = du4338 v2
du4338 ::
  MAlonzo.Code.Function.Injection.T88 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Relation.Nullary.T14
du4338 v0
  = coe
      (MAlonzo.Code.Relation.Nullary.Decidable.du54 (coe v0) (coe d1268))
name4342 = "Data.Nat.Properties._*-mono_"
d4342 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4342 = coe d2300
name4344 = "Data.Nat.Properties._+-mono_"
d4344 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4344 = coe d2000
name4346 = "Data.Nat.Properties.+-right-identity"
d4346 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4346 = erased
name4348 = "Data.Nat.Properties.*-right-zero"
d4348 :: Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4348 = erased
name4350 = "Data.Nat.Properties.distribʳ-*-+"
d4350 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4350 = erased
name4352 = "Data.Nat.Properties.*-distrib-∸ʳ"
d4352 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4352 = erased
name4354 = "Data.Nat.Properties.cancel-+-left"
d4354 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4354 = erased
name4356 = "Data.Nat.Properties.cancel-+-left-≤"
d4356 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4356 = coe d1878
name4358 = "Data.Nat.Properties.cancel-*-right"
d4358 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4358 = erased
name4360 = "Data.Nat.Properties.cancel-*-right-≤"
d4360 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4360 = coe d2290
name4362 = "Data.Nat.Properties.strictTotalOrder"
d4362 :: MAlonzo.Code.Relation.Binary.T1268
d4362 = coe d1628
name4364 = "Data.Nat.Properties.isCommutativeSemiring"
d4364 :: MAlonzo.Code.Algebra.Structures.T1192
d4364 = coe d2196
name4366 = "Data.Nat.Properties.commutativeSemiring"
d4366 :: MAlonzo.Code.Algebra.T1690
d4366 = coe d2208
name4368 = "Data.Nat.Properties.isDistributiveLattice"
d4368 :: MAlonzo.Code.Algebra.Structures.T692
d4368 = coe d3114
name4370 = "Data.Nat.Properties.distributiveLattice"
d4370 :: MAlonzo.Code.Algebra.T950
d4370 = coe d3128
name4372 = "Data.Nat.Properties.⊔-⊓-0-isSemiringWithoutOne"
d4372 :: MAlonzo.Code.Algebra.Structures.T826
d4372 = coe d3108
name4374
  = "Data.Nat.Properties.⊔-⊓-0-isCommutativeSemiringWithoutOne"
d4374 :: MAlonzo.Code.Algebra.Structures.T914
d4374 = coe d3110
name4376
  = "Data.Nat.Properties.⊔-⊓-0-commutativeSemiringWithoutOne"
d4376 :: MAlonzo.Code.Algebra.T1254
d4376 = coe d3124
name4378 = "Data.Nat.Properties.¬i+1+j≤i"
d4378 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d4378 = erased
name4380 = "Data.Nat.Properties.≤-steps"
d4380 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
d4380 v0 v1 v2 v3 = du4380 v3
du4380 ::
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Nat.Base.T6
du4380 v0 = coe v0
name4388 = "Data.Nat.Properties.i∸k∸j+j∸k≡i+j∸k"
d4388 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4388 = erased
name4424 = "Data.Nat.Properties.im≡jm+n⇒[i∸j]m≡n"
d4424 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4424 = erased
name4440 = "Data.Nat.Properties.≤+≢⇒<"
d4440 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T12 ->
   MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Data.Nat.Base.T6
d4440 = coe d1504
name4442 = "Data.Nat.Properties.≤-irrelevance"
d4442 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4442 = erased
name4444 = "Data.Nat.Properties.<-irrelevance"
d4444 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Data.Nat.Base.T6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4444 = erased
name4446 = "Data.Nat.Properties.i+1+j≢i"
d4446 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d4446 = erased
name4448 = "Data.Nat.Properties.i+j≡0⇒i≡0"
d4448 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4448 = erased
name4450 = "Data.Nat.Properties.i+j≡0⇒j≡0"
d4450 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4450 = erased
name4452 = "Data.Nat.Properties.i+1+j≰i"
d4452 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T6 -> MAlonzo.Code.Data.Empty.T4
d4452 = erased
name4454 = "Data.Nat.Properties.i*j≡0⇒i≡0∨j≡0"
d4454 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Sum.Base.T30
d4454 = coe d2248
name4456 = "Data.Nat.Properties.i*j≡1⇒i≡1"
d4456 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4456 = erased
name4458 = "Data.Nat.Properties.i*j≡1⇒j≡1"
d4458 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4458 = erased
name4460 = "Data.Nat.Properties.i^j≡0⇒i≡0"
d4460 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4460 = erased
name4462 = "Data.Nat.Properties.i^j≡1⇒j≡0∨i≡1"
d4462 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Sum.Base.T30
d4462 = coe d2504
name4464 = "Data.Nat.Properties.[i+j]∸[i+k]≡j∸k"
d4464 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4464 = erased
name4466 = "Data.Nat.Properties.m≢0⇒suc[pred[m]]≡m"
d4466 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T12 ->
   MAlonzo.Code.Data.Empty.T4) ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4466 = erased
name4468 = "Data.Nat.Properties.n≡m⇒∣n-m∣≡0"
d4468 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4468 = erased
name4470 = "Data.Nat.Properties.∣n-m∣≡0⇒n≡m"
d4470 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12
d4470 = erased
name4472 = "Data.Nat.Properties.∣n-m∣≡n∸m⇒m≤n"
d4472 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Nat.Base.T6
d4472 = coe d3920
name4474 = "Data.Nat.Properties.∣n-n+m∣≡m"
d4474 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4474 = erased
name4476 = "Data.Nat.Properties.∣n+m-n+o∣≡∣m-o|"
d4476 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Equality.T12
d4476 = erased
name4478 = "Data.Nat.Properties.n∸m≤∣n-m∣"
d4478 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d4478 = coe d3974
name4480 = "Data.Nat.Properties.∣n-m∣≤n⊔m"
d4480 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d4480 = coe d4004
name4486 = "Data.Nat.Properties.n≤m+n"
d4486 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d4486 v0 v1 = du4486 v1
du4486 :: Integer -> MAlonzo.Code.Data.Nat.Base.T6
du4486 v0 = coe (du1952 (coe v0))
name4498 = "Data.Nat.Properties.n≤m+n∸m"
d4498 :: Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T6
d4498 v0 v1
  = case coe v1 of
      0 -> coe (\ v2 -> MAlonzo.Code.Data.Nat.Base.C10) erased
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           case coe v0 of
             0 -> coe (d1328 (coe v1))
             _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
                  coe
                    (\ v4 v5 v6 -> MAlonzo.Code.Data.Nat.Base.C18 v6) erased erased
                    (d4498 (coe v3) (coe v2))
name4512 = "Data.Nat.Properties.∣n-m∣≡[n∸m]∨[m∸n]"
d4512 :: Integer -> Integer -> MAlonzo.Code.Data.Sum.Base.T30
d4512 v0 v1
  = let v2 = d1342 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C38 v3
        -> coe (MAlonzo.Code.Data.Sum.Base.C38 erased)
      MAlonzo.Code.Data.Sum.Base.C42 v3
        -> coe
             (MAlonzo.Code.Data.Sum.Base.C42
                (coe
                   (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du190
                      (coe
                         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.C84
                            (coe
                               MAlonzo.Code.Relation.Binary.Core.d414
                               (MAlonzo.Code.Relation.Binary.d36 (coe d1394))
                               (coe MAlonzo.Code.Agda.Builtin.Nat.d22 v0 v1)))))))
      _ -> MAlonzo.RTE.mazUnreachableError
name29479 = "Data.Nat.Properties..absurdlambda"
d29479 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T12 ->
  MAlonzo.Code.Data.Empty.T4
d29479 = erased
