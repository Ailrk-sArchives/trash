{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Algebra where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary
import qualified MAlonzo.Code.Relation.Binary.Core

name6 = "Algebra.RawMagma"
d6 a0 a1 = ()
newtype T6 = C3 (AgdaAny -> AgdaAny -> AgdaAny)
name18 = "Algebra.RawMagma.Carrier"
d18 :: T6 -> ()
d18 = erased
name20 = "Algebra.RawMagma._≈_"
d20 :: T6 -> AgdaAny -> AgdaAny -> ()
d20 = erased
name22 = "Algebra.RawMagma._∙_"
d22 :: T6 -> AgdaAny -> AgdaAny -> AgdaAny
d22 v0
  = case coe v0 of
      C3 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name28 = "Algebra.Magma"
d28 a0 a1 = ()
data T28
  = C77 (AgdaAny -> AgdaAny -> AgdaAny)
        MAlonzo.Code.Algebra.Structures.T84
name42 = "Algebra.Magma.Carrier"
d42 :: T28 -> ()
d42 = erased
name44 = "Algebra.Magma._≈_"
d44 :: T28 -> AgdaAny -> AgdaAny -> ()
d44 = erased
name46 = "Algebra.Magma._∙_"
d46 :: T28 -> AgdaAny -> AgdaAny -> AgdaAny
d46 v0
  = case coe v0 of
      C77 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name48 = "Algebra.Magma.isMagma"
d48 :: T28 -> MAlonzo.Code.Algebra.Structures.T84
d48 v0
  = case coe v0 of
      C77 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name52 = "Algebra.Magma._.isEquivalence"
d52 :: T28 -> MAlonzo.Code.Relation.Binary.Core.T402
d52 v0
  = coe (MAlonzo.Code.Algebra.Structures.d92 (coe (d48 (coe v0))))
name54 = "Algebra.Magma._.refl"
d54 :: T28 -> AgdaAny -> AgdaAny
d54 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe (MAlonzo.Code.Algebra.Structures.d92 (coe (d48 (coe v0))))))
name56 = "Algebra.Magma._.reflexive"
d56 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T28 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d56 v0 v1 v2 = du56 v2
du56 ::
  T28 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du56 v0
  = let v1 = d48 (coe v0) in
    coe
      (\ v2 v3 v4 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v1))) v2)
name58 = "Algebra.Magma._.setoid"
d58 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T28 -> MAlonzo.Code.Relation.Binary.T128
d58 v0 v1 v2 = du58 v2
du58 :: T28 -> MAlonzo.Code.Relation.Binary.T128
du58 v0
  = coe (MAlonzo.Code.Algebra.Structures.du106 (coe (d48 (coe v0))))
name60 = "Algebra.Magma._.sym"
d60 :: T28 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d60 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe (MAlonzo.Code.Algebra.Structures.d92 (coe (d48 (coe v0))))))
name62 = "Algebra.Magma._.trans"
d62 ::
  T28 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d62 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe (MAlonzo.Code.Algebra.Structures.d92 (coe (d48 (coe v0))))))
name64 = "Algebra.Magma._.∙-cong"
d64 ::
  T28 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d64 v0
  = coe (MAlonzo.Code.Algebra.Structures.d94 (coe (d48 (coe v0))))
name66 = "Algebra.Magma._.∙-congʳ"
d66 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T28 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d66 v0 v1 v2 = du66 v2
du66 :: T28 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du66 v0
  = coe (MAlonzo.Code.Algebra.Structures.du112 (coe (d48 (coe v0))))
name68 = "Algebra.Magma._.∙-congˡ"
d68 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T28 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d68 v0 v1 v2 = du68 v2
du68 :: T28 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du68 v0
  = coe (MAlonzo.Code.Algebra.Structures.du108 (coe (d48 (coe v0))))
name70 = "Algebra.Magma.rawMagma"
d70 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T28 -> T6
d70 v0 v1 v2 = du70 v2
du70 :: T28 -> T6
du70 v0 = coe (\ v1 v2 v3 -> C3 v3) erased erased (d46 (coe v0))
name76 = "Algebra.Semigroup"
d76 a0 a1 = ()
data T76
  = C285 (AgdaAny -> AgdaAny -> AgdaAny)
         MAlonzo.Code.Algebra.Structures.T118
name90 = "Algebra.Semigroup.Carrier"
d90 :: T76 -> ()
d90 = erased
name92 = "Algebra.Semigroup._≈_"
d92 :: T76 -> AgdaAny -> AgdaAny -> ()
d92 = erased
name94 = "Algebra.Semigroup._∙_"
d94 :: T76 -> AgdaAny -> AgdaAny -> AgdaAny
d94 v0
  = case coe v0 of
      C285 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name96 = "Algebra.Semigroup.isSemigroup"
d96 :: T76 -> MAlonzo.Code.Algebra.Structures.T118
d96 v0
  = case coe v0 of
      C285 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name100 = "Algebra.Semigroup._.assoc"
d100 :: T76 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d100 v0
  = coe (MAlonzo.Code.Algebra.Structures.d128 (coe (d96 (coe v0))))
name102 = "Algebra.Semigroup._.isEquivalence"
d102 :: T76 -> MAlonzo.Code.Relation.Binary.Core.T402
d102 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))))
name104 = "Algebra.Semigroup._.isMagma"
d104 :: T76 -> MAlonzo.Code.Algebra.Structures.T84
d104 v0
  = coe (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))
name106 = "Algebra.Semigroup._.refl"
d106 :: T76 -> AgdaAny -> AgdaAny
d106 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))))))
name108 = "Algebra.Semigroup._.reflexive"
d108 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T76 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d108 v0 v1 v2 = du108 v2
du108 ::
  T76 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du108 v0
  = let v1 = d96 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d126 (coe v1) in
    coe
      (\ v3 v4 v5 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v2))) v3)
name110 = "Algebra.Semigroup._.setoid"
d110 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T76 -> MAlonzo.Code.Relation.Binary.T128
d110 v0 v1 v2 = du110 v2
du110 :: T76 -> MAlonzo.Code.Relation.Binary.T128
du110 v0
  = let v1 = d96 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v1))))
name112 = "Algebra.Semigroup._.sym"
d112 :: T76 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d112 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))))))
name114 = "Algebra.Semigroup._.trans"
d114 ::
  T76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d114 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))))))
name116 = "Algebra.Semigroup._.∙-cong"
d116 ::
  T76 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d116 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))))
name118 = "Algebra.Semigroup._.∙-congʳ"
d118 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T76 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d118 v0 v1 v2 = du118 v2
du118 :: T76 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du118 v0
  = let v1 = d96 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v1))))
name120 = "Algebra.Semigroup._.∙-congˡ"
d120 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T76 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d120 v0 v1 v2 = du120 v2
du120 :: T76 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du120 v0
  = let v1 = d96 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v1))))
name122 = "Algebra.Semigroup.magma"
d122 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T76 -> T28
d122 v0 v1 v2 = du122 v2
du122 :: T76 -> T28
du122 v0
  = coe
      (\ v1 v2 v3 v4 -> C77 v3 v4) erased erased (d94 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d126 (coe (d96 (coe v0))))
name126 = "Algebra.Semigroup._.rawMagma"
d126 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T76 -> T6
d126 v0 v1 v2 = du126 v2
du126 :: T76 -> T6
du126 v0 = coe (du70 (coe (du122 (coe v0))))
name132 = "Algebra.Band"
d132 a0 a1 = ()
data T132
  = C515 (AgdaAny -> AgdaAny -> AgdaAny)
         MAlonzo.Code.Algebra.Structures.T152
name146 = "Algebra.Band.Carrier"
d146 :: T132 -> ()
d146 = erased
name148 = "Algebra.Band._≈_"
d148 :: T132 -> AgdaAny -> AgdaAny -> ()
d148 = erased
name150 = "Algebra.Band._∙_"
d150 :: T132 -> AgdaAny -> AgdaAny -> AgdaAny
d150 v0
  = case coe v0 of
      C515 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name152 = "Algebra.Band.isBand"
d152 :: T132 -> MAlonzo.Code.Algebra.Structures.T152
d152 v0
  = case coe v0 of
      C515 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name156 = "Algebra.Band._.assoc"
d156 :: T132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d156 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))
name158 = "Algebra.Band._.idem"
d158 :: T132 -> AgdaAny -> AgdaAny
d158 v0
  = coe (MAlonzo.Code.Algebra.Structures.d162 (coe (d152 (coe v0))))
name160 = "Algebra.Band._.isEquivalence"
d160 :: T132 -> MAlonzo.Code.Relation.Binary.Core.T402
d160 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))))
name162 = "Algebra.Band._.isMagma"
d162 :: T132 -> MAlonzo.Code.Algebra.Structures.T84
d162 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))
name164 = "Algebra.Band._.isSemigroup"
d164 :: T132 -> MAlonzo.Code.Algebra.Structures.T118
d164 v0
  = coe (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))
name166 = "Algebra.Band._.refl"
d166 :: T132 -> AgdaAny -> AgdaAny
d166 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))))))
name168 = "Algebra.Band._.reflexive"
d168 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T132 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d168 v0 v1 v2 = du168 v2
du168 ::
  T132 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du168 v0
  = let v1 = d152 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d160 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d126 (coe v2) in
    coe
      (\ v4 v5 v6 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v3))) v4)
name170 = "Algebra.Band._.setoid"
d170 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T132 -> MAlonzo.Code.Relation.Binary.T128
d170 v0 v1 v2 = du170 v2
du170 :: T132 -> MAlonzo.Code.Relation.Binary.T128
du170 v0
  = let v1 = d152 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d160 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name172 = "Algebra.Band._.sym"
d172 :: T132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d172 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))))))
name174 = "Algebra.Band._.trans"
d174 ::
  T132 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d174 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))))))
name176 = "Algebra.Band._.∙-cong"
d176 ::
  T132 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d176 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))))))
name178 = "Algebra.Band._.∙-congʳ"
d178 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d178 v0 v1 v2 = du178 v2
du178 ::
  T132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du178 v0
  = let v1 = d152 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d160 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name180 = "Algebra.Band._.∙-congˡ"
d180 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d180 v0 v1 v2 = du180 v2
du180 ::
  T132 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du180 v0
  = let v1 = d152 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d160 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name182 = "Algebra.Band.semigroup"
d182 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T132 -> T76
d182 v0 v1 v2 = du182 v2
du182 :: T132 -> T76
du182 v0
  = coe
      (\ v1 v2 v3 v4 -> C285 v3 v4) erased erased (d150 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d160 (coe (d152 (coe v0))))
name186 = "Algebra.Band._.magma"
d186 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T132 -> T28
d186 v0 v1 v2 = du186 v2
du186 :: T132 -> T28
du186 v0 = coe (du122 (coe (du182 (coe v0))))
name188 = "Algebra.Band._.rawMagma"
d188 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T132 -> T6
d188 v0 v1 v2 = du188 v2
du188 :: T132 -> T6
du188 v0
  = let v1 = du182 (coe v0) in coe (du70 (coe (du122 (coe v1))))
name194 = "Algebra.Semilattice"
d194 a0 a1 = ()
data T194
  = C771 (AgdaAny -> AgdaAny -> AgdaAny)
         MAlonzo.Code.Algebra.Structures.T190
name208 = "Algebra.Semilattice.Carrier"
d208 :: T194 -> ()
d208 = erased
name210 = "Algebra.Semilattice._≈_"
d210 :: T194 -> AgdaAny -> AgdaAny -> ()
d210 = erased
name212 = "Algebra.Semilattice._∧_"
d212 :: T194 -> AgdaAny -> AgdaAny -> AgdaAny
d212 v0
  = case coe v0 of
      C771 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name214 = "Algebra.Semilattice.isSemilattice"
d214 :: T194 -> MAlonzo.Code.Algebra.Structures.T190
d214 v0
  = case coe v0 of
      C771 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name218 = "Algebra.Semilattice._.assoc"
d218 :: T194 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d218 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d160
               (coe
                  (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))))))
name220 = "Algebra.Semilattice._.comm"
d220 :: T194 -> AgdaAny -> AgdaAny -> AgdaAny
d220 v0
  = coe (MAlonzo.Code.Algebra.Structures.d200 (coe (d214 (coe v0))))
name222 = "Algebra.Semilattice._.idem"
d222 :: T194 -> AgdaAny -> AgdaAny
d222 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d162
         (coe (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))))
name224 = "Algebra.Semilattice._.isBand"
d224 :: T194 -> MAlonzo.Code.Algebra.Structures.T152
d224 v0
  = coe (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))
name226 = "Algebra.Semilattice._.isEquivalence"
d226 :: T194 -> MAlonzo.Code.Relation.Binary.Core.T402
d226 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d160
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))))))))
name228 = "Algebra.Semilattice._.isMagma"
d228 :: T194 -> MAlonzo.Code.Algebra.Structures.T84
d228 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d160
               (coe
                  (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))))))
name230 = "Algebra.Semilattice._.isSemigroup"
d230 :: T194 -> MAlonzo.Code.Algebra.Structures.T118
d230 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d160
         (coe (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))))
name232 = "Algebra.Semilattice._.refl"
d232 :: T194 -> AgdaAny -> AgdaAny
d232 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d160
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d198
                                 (coe (d214 (coe v0))))))))))))
name234 = "Algebra.Semilattice._.reflexive"
d234 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T194 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d234 v0 v1 v2 = du234 v2
du234 ::
  T194 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du234 v0
  = let v1 = d214 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d198 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d160 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d126 (coe v3) in
    coe
      (\ v5 v6 v7 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v4))) v5)
name236 = "Algebra.Semilattice._.setoid"
d236 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T194 -> MAlonzo.Code.Relation.Binary.T128
d236 v0 v1 v2 = du236 v2
du236 :: T194 -> MAlonzo.Code.Relation.Binary.T128
du236 v0
  = let v1 = d214 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d198 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d160 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name238 = "Algebra.Semilattice._.sym"
d238 :: T194 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d238 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d160
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d198
                                 (coe (d214 (coe v0))))))))))))
name240 = "Algebra.Semilattice._.trans"
d240 ::
  T194 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d240 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d160
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d198
                                 (coe (d214 (coe v0))))))))))))
name242 = "Algebra.Semilattice._.∙-cong"
d242 ::
  T194 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d242 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d160
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))))))))
name244 = "Algebra.Semilattice._.∙-congʳ"
d244 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T194 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d244 v0 v1 v2 = du244 v2
du244 ::
  T194 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du244 v0
  = let v1 = d214 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d198 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d160 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name246 = "Algebra.Semilattice._.∙-congˡ"
d246 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T194 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d246 v0 v1 v2 = du246 v2
du246 ::
  T194 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du246 v0
  = let v1 = d214 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d198 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d160 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name248 = "Algebra.Semilattice.band"
d248 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T194 -> T132
d248 v0 v1 v2 = du248 v2
du248 :: T194 -> T132
du248 v0
  = coe
      (\ v1 v2 v3 v4 -> C515 v3 v4) erased erased (d212 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d198 (coe (d214 (coe v0))))
name252 = "Algebra.Semilattice._.magma"
d252 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T194 -> T28
d252 v0 v1 v2 = du252 v2
du252 :: T194 -> T28
du252 v0
  = let v1 = du248 (coe v0) in coe (du122 (coe (du182 (coe v1))))
name254 = "Algebra.Semilattice._.rawMagma"
d254 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T194 -> T6
d254 v0 v1 v2 = du254 v2
du254 :: T194 -> T6
du254 v0
  = let v1 = du248 (coe v0) in
    let v2 = du182 (coe v1) in coe (du70 (coe (du122 (coe v2))))
name256 = "Algebra.Semilattice._.semigroup"
d256 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T194 -> T76
d256 v0 v1 v2 = du256 v2
du256 :: T194 -> T76
du256 v0 = coe (du182 (coe (du248 (coe v0))))
name262 = "Algebra.SelectiveMagma"
d262 a0 a1 = ()
data T262
  = C1053 (AgdaAny -> AgdaAny -> AgdaAny)
          MAlonzo.Code.Algebra.Structures.T232
name276 = "Algebra.SelectiveMagma.Carrier"
d276 :: T262 -> ()
d276 = erased
name278 = "Algebra.SelectiveMagma._≈_"
d278 :: T262 -> AgdaAny -> AgdaAny -> ()
d278 = erased
name280 = "Algebra.SelectiveMagma._∙_"
d280 :: T262 -> AgdaAny -> AgdaAny -> AgdaAny
d280 v0
  = case coe v0 of
      C1053 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name282 = "Algebra.SelectiveMagma.isSelectiveMagma"
d282 :: T262 -> MAlonzo.Code.Algebra.Structures.T232
d282 v0
  = case coe v0 of
      C1053 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name286 = "Algebra.SelectiveMagma._.isEquivalence"
d286 :: T262 -> MAlonzo.Code.Relation.Binary.Core.T402
d286 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))))
name288 = "Algebra.SelectiveMagma._.isMagma"
d288 :: T262 -> MAlonzo.Code.Algebra.Structures.T84
d288 v0
  = coe (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))
name290 = "Algebra.SelectiveMagma._.refl"
d290 :: T262 -> AgdaAny -> AgdaAny
d290 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))))))
name292 = "Algebra.SelectiveMagma._.reflexive"
d292 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T262 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d292 v0 v1 v2 = du292 v2
du292 ::
  T262 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du292 v0
  = let v1 = d282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d240 (coe v1) in
    coe
      (\ v3 v4 v5 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v2))) v3)
name294 = "Algebra.SelectiveMagma._.sel"
d294 ::
  T262 -> AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T30
d294 v0
  = coe (MAlonzo.Code.Algebra.Structures.d242 (coe (d282 (coe v0))))
name296 = "Algebra.SelectiveMagma._.setoid"
d296 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T262 -> MAlonzo.Code.Relation.Binary.T128
d296 v0 v1 v2 = du296 v2
du296 :: T262 -> MAlonzo.Code.Relation.Binary.T128
du296 v0
  = let v1 = d282 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d240 (coe v1))))
name298 = "Algebra.SelectiveMagma._.sym"
d298 :: T262 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d298 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))))))
name300 = "Algebra.SelectiveMagma._.trans"
d300 ::
  T262 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d300 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))))))
name302 = "Algebra.SelectiveMagma._.∙-cong"
d302 ::
  T262 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d302 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))))
name304 = "Algebra.SelectiveMagma._.∙-congʳ"
d304 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T262 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d304 v0 v1 v2 = du304 v2
du304 ::
  T262 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du304 v0
  = let v1 = d282 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d240 (coe v1))))
name306 = "Algebra.SelectiveMagma._.∙-congˡ"
d306 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T262 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d306 v0 v1 v2 = du306 v2
du306 ::
  T262 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du306 v0
  = let v1 = d282 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d240 (coe v1))))
name308 = "Algebra.SelectiveMagma.magma"
d308 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T262 -> T28
d308 v0 v1 v2 = du308 v2
du308 :: T262 -> T28
du308 v0
  = coe
      (\ v1 v2 v3 v4 -> C77 v3 v4) erased erased (d280 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d240 (coe (d282 (coe v0))))
name312 = "Algebra.SelectiveMagma._.rawMagma"
d312 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T262 -> T6
d312 v0 v1 v2 = du312 v2
du312 :: T262 -> T6
du312 v0 = coe (du70 (coe (du308 (coe v0))))
name318 = "Algebra.RawMonoid"
d318 a0 a1 = ()
data T318 = C1275 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
name332 = "Algebra.RawMonoid.Carrier"
d332 :: T318 -> ()
d332 = erased
name334 = "Algebra.RawMonoid._≈_"
d334 :: T318 -> AgdaAny -> AgdaAny -> ()
d334 = erased
name336 = "Algebra.RawMonoid._∙_"
d336 :: T318 -> AgdaAny -> AgdaAny -> AgdaAny
d336 v0
  = case coe v0 of
      C1275 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name338 = "Algebra.RawMonoid.ε"
d338 :: T318 -> AgdaAny
d338 v0
  = case coe v0 of
      C1275 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name344 = "Algebra.Monoid"
d344 a0 a1 = ()
data T344
  = C1365 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          MAlonzo.Code.Algebra.Structures.T268
name360 = "Algebra.Monoid.Carrier"
d360 :: T344 -> ()
d360 = erased
name362 = "Algebra.Monoid._≈_"
d362 :: T344 -> AgdaAny -> AgdaAny -> ()
d362 = erased
name364 = "Algebra.Monoid._∙_"
d364 :: T344 -> AgdaAny -> AgdaAny -> AgdaAny
d364 v0
  = case coe v0 of
      C1365 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name366 = "Algebra.Monoid.ε"
d366 :: T344 -> AgdaAny
d366 v0
  = case coe v0 of
      C1365 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name368 = "Algebra.Monoid.isMonoid"
d368 :: T344 -> MAlonzo.Code.Algebra.Structures.T268
d368 v0
  = case coe v0 of
      C1365 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name372 = "Algebra.Monoid._.assoc"
d372 :: T344 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d372 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))
name374 = "Algebra.Monoid._.identity"
d374 :: T344 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d374 v0
  = coe (MAlonzo.Code.Algebra.Structures.d280 (coe (d368 (coe v0))))
name376 = "Algebra.Monoid._.identityʳ"
d376 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T344 -> AgdaAny -> AgdaAny
d376 v0 v1 v2 = du376 v2
du376 :: T344 -> AgdaAny -> AgdaAny
du376 v0
  = let v1 = d368 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v1))))
name378 = "Algebra.Monoid._.identityˡ"
d378 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T344 -> AgdaAny -> AgdaAny
d378 v0 v1 v2 = du378 v2
du378 :: T344 -> AgdaAny -> AgdaAny
du378 v0
  = let v1 = d368 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v1))))
name380 = "Algebra.Monoid._.isEquivalence"
d380 :: T344 -> MAlonzo.Code.Relation.Binary.Core.T402
d380 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))))
name382 = "Algebra.Monoid._.isMagma"
d382 :: T344 -> MAlonzo.Code.Algebra.Structures.T84
d382 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))
name384 = "Algebra.Monoid._.isSemigroup"
d384 :: T344 -> MAlonzo.Code.Algebra.Structures.T118
d384 v0
  = coe (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))
name386 = "Algebra.Monoid._.refl"
d386 :: T344 -> AgdaAny -> AgdaAny
d386 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))))))
name388 = "Algebra.Monoid._.reflexive"
d388 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T344 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d388 v0 v1 v2 = du388 v2
du388 ::
  T344 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du388 v0
  = let v1 = d368 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d278 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d126 (coe v2) in
    coe
      (\ v4 v5 v6 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v3))) v4)
name390 = "Algebra.Monoid._.setoid"
d390 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T344 -> MAlonzo.Code.Relation.Binary.T128
d390 v0 v1 v2 = du390 v2
du390 :: T344 -> MAlonzo.Code.Relation.Binary.T128
du390 v0
  = let v1 = d368 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d278 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name392 = "Algebra.Monoid._.sym"
d392 :: T344 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d392 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))))))
name394 = "Algebra.Monoid._.trans"
d394 ::
  T344 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d394 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))))))
name396 = "Algebra.Monoid._.∙-cong"
d396 ::
  T344 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d396 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))))))
name398 = "Algebra.Monoid._.∙-congʳ"
d398 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T344 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d398 v0 v1 v2 = du398 v2
du398 ::
  T344 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du398 v0
  = let v1 = d368 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d278 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name400 = "Algebra.Monoid._.∙-congˡ"
d400 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T344 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d400 v0 v1 v2 = du400 v2
du400 ::
  T344 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du400 v0
  = let v1 = d368 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d278 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name402 = "Algebra.Monoid.semigroup"
d402 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T344 -> T76
d402 v0 v1 v2 = du402 v2
du402 :: T344 -> T76
du402 v0
  = coe
      (\ v1 v2 v3 v4 -> C285 v3 v4) erased erased (d364 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d278 (coe (d368 (coe v0))))
name404 = "Algebra.Monoid.rawMonoid"
d404 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T344 -> T318
d404 v0 v1 v2 = du404 v2
du404 :: T344 -> T318
du404 v0
  = coe
      (\ v1 v2 v3 v4 -> C1275 v3 v4) erased erased (d364 (coe v0))
      (d366 (coe v0))
name408 = "Algebra.Monoid._.magma"
d408 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T344 -> T28
d408 v0 v1 v2 = du408 v2
du408 :: T344 -> T28
du408 v0 = coe (du122 (coe (du402 (coe v0))))
name410 = "Algebra.Monoid._.rawMagma"
d410 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T344 -> T6
d410 v0 v1 v2 = du410 v2
du410 :: T344 -> T6
du410 v0
  = let v1 = du402 (coe v0) in coe (du70 (coe (du122 (coe v1))))
name416 = "Algebra.CommutativeMonoid"
d416 a0 a1 = ()
data T416
  = C1695 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          MAlonzo.Code.Algebra.Structures.T314
name432 = "Algebra.CommutativeMonoid.Carrier"
d432 :: T416 -> ()
d432 = erased
name434 = "Algebra.CommutativeMonoid._≈_"
d434 :: T416 -> AgdaAny -> AgdaAny -> ()
d434 = erased
name436 = "Algebra.CommutativeMonoid._∙_"
d436 :: T416 -> AgdaAny -> AgdaAny -> AgdaAny
d436 v0
  = case coe v0 of
      C1695 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name438 = "Algebra.CommutativeMonoid.ε"
d438 :: T416 -> AgdaAny
d438 v0
  = case coe v0 of
      C1695 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name440 = "Algebra.CommutativeMonoid.isCommutativeMonoid"
d440 :: T416 -> MAlonzo.Code.Algebra.Structures.T314
d440 v0
  = case coe v0 of
      C1695 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name444 = "Algebra.CommutativeMonoid._.assoc"
d444 :: T416 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d444 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))
name446 = "Algebra.CommutativeMonoid._.comm"
d446 :: T416 -> AgdaAny -> AgdaAny -> AgdaAny
d446 v0
  = coe (MAlonzo.Code.Algebra.Structures.d330 (coe (d440 (coe v0))))
name448 = "Algebra.CommutativeMonoid._.identity"
d448 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T416 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d448 v0 v1 v2 = du448 v2
du448 :: T416 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du448 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe (d436 (coe v0))) (coe (d438 (coe v0))) (coe (d440 (coe v0))))
name450 = "Algebra.CommutativeMonoid._.identityʳ"
d450 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T416 -> AgdaAny -> AgdaAny
d450 v0 v1 v2 = du450 v2
du450 :: T416 -> AgdaAny -> AgdaAny
du450 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du356
         (coe (d436 (coe v0))) (coe (d438 (coe v0))) (coe (d440 (coe v0))))
name452 = "Algebra.CommutativeMonoid._.identityˡ"
d452 :: T416 -> AgdaAny -> AgdaAny
d452 v0
  = coe (MAlonzo.Code.Algebra.Structures.d328 (coe (d440 (coe v0))))
name454 = "Algebra.CommutativeMonoid._.isEquivalence"
d454 :: T416 -> MAlonzo.Code.Relation.Binary.Core.T402
d454 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))))
name456 = "Algebra.CommutativeMonoid._.isMagma"
d456 :: T416 -> MAlonzo.Code.Algebra.Structures.T84
d456 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))
name458 = "Algebra.CommutativeMonoid._.isMonoid"
d458 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T416 -> MAlonzo.Code.Algebra.Structures.T268
d458 v0 v1 v2 = du458 v2
du458 :: T416 -> MAlonzo.Code.Algebra.Structures.T268
du458 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe (d436 (coe v0))) (coe (d438 (coe v0))) (coe (d440 (coe v0))))
name460 = "Algebra.CommutativeMonoid._.isSemigroup"
d460 :: T416 -> MAlonzo.Code.Algebra.Structures.T118
d460 v0
  = coe (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))
name462 = "Algebra.CommutativeMonoid._.refl"
d462 :: T416 -> AgdaAny -> AgdaAny
d462 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))))))
name464 = "Algebra.CommutativeMonoid._.reflexive"
d464 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T416 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d464 v0 v1 v2 = du464 v2
du464 ::
  T416 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du464 v0
  = let v1 = d440 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d326 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d126 (coe v2) in
    coe
      (\ v4 v5 v6 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v3))) v4)
name466 = "Algebra.CommutativeMonoid._.setoid"
d466 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T416 -> MAlonzo.Code.Relation.Binary.T128
d466 v0 v1 v2 = du466 v2
du466 :: T416 -> MAlonzo.Code.Relation.Binary.T128
du466 v0
  = let v1 = d440 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d326 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name468 = "Algebra.CommutativeMonoid._.sym"
d468 :: T416 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d468 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))))))
name470 = "Algebra.CommutativeMonoid._.trans"
d470 ::
  T416 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d470 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))))))
name472 = "Algebra.CommutativeMonoid._.∙-cong"
d472 ::
  T416 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d472 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326 (coe (d440 (coe v0))))))))
name474 = "Algebra.CommutativeMonoid._.∙-congʳ"
d474 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T416 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d474 v0 v1 v2 = du474 v2
du474 ::
  T416 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du474 v0
  = let v1 = d440 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d326 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name476 = "Algebra.CommutativeMonoid._.∙-congˡ"
d476 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T416 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d476 v0 v1 v2 = du476 v2
du476 ::
  T416 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du476 v0
  = let v1 = d440 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d326 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name478 = "Algebra.CommutativeMonoid.monoid"
d478 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T416 -> T344
d478 v0 v1 v2 = du478 v2
du478 :: T416 -> T344
du478 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1365 v3 v4 v5) erased erased (d436 (coe v0))
      (d438 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du360
         (coe (d436 (coe v0))) (coe (d438 (coe v0))) (coe (d440 (coe v0))))
name482 = "Algebra.CommutativeMonoid._.magma"
d482 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T416 -> T28
d482 v0 v1 v2 = du482 v2
du482 :: T416 -> T28
du482 v0
  = let v1 = du478 (coe v0) in coe (du122 (coe (du402 (coe v1))))
name484 = "Algebra.CommutativeMonoid._.rawMagma"
d484 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T416 -> T6
d484 v0 v1 v2 = du484 v2
du484 :: T416 -> T6
du484 v0
  = let v1 = du478 (coe v0) in
    let v2 = du402 (coe v1) in coe (du70 (coe (du122 (coe v2))))
name486 = "Algebra.CommutativeMonoid._.rawMonoid"
d486 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T416 -> T318
d486 v0 v1 v2 = du486 v2
du486 :: T416 -> T318
du486 v0 = coe (du404 (coe (du478 (coe v0))))
name488 = "Algebra.CommutativeMonoid._.semigroup"
d488 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T416 -> T76
d488 v0 v1 v2 = du488 v2
du488 :: T416 -> T76
du488 v0 = coe (du402 (coe (du478 (coe v0))))
name494 = "Algebra.IdempotentCommutativeMonoid"
d494 a0 a1 = ()
data T494
  = C2007 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          MAlonzo.Code.Algebra.Structures.T366
name510 = "Algebra.IdempotentCommutativeMonoid.Carrier"
d510 :: T494 -> ()
d510 = erased
name512 = "Algebra.IdempotentCommutativeMonoid._≈_"
d512 :: T494 -> AgdaAny -> AgdaAny -> ()
d512 = erased
name514 = "Algebra.IdempotentCommutativeMonoid._∙_"
d514 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny
d514 v0
  = case coe v0 of
      C2007 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name516 = "Algebra.IdempotentCommutativeMonoid.ε"
d516 :: T494 -> AgdaAny
d516 v0
  = case coe v0 of
      C2007 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name518
  = "Algebra.IdempotentCommutativeMonoid.isIdempotentCommutativeMonoid"
d518 :: T494 -> MAlonzo.Code.Algebra.Structures.T366
d518 v0
  = case coe v0 of
      C2007 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name522 = "Algebra.IdempotentCommutativeMonoid._.assoc"
d522 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d522 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))
name524 = "Algebra.IdempotentCommutativeMonoid._.comm"
d524 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny
d524 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))
name526 = "Algebra.IdempotentCommutativeMonoid._.idem"
d526 :: T494 -> AgdaAny -> AgdaAny
d526 v0
  = coe (MAlonzo.Code.Algebra.Structures.d378 (coe (d518 (coe v0))))
name528 = "Algebra.IdempotentCommutativeMonoid._.identity"
d528 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d528 v0 v1 v2 = du528 v2
du528 :: T494 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du528 v0
  = let v1 = d514 (coe v0) in
    let v2 = d516 (coe v0) in
    let v3 = d518 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe v3))))
name530 = "Algebra.IdempotentCommutativeMonoid._.identityʳ"
d530 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> AgdaAny -> AgdaAny
d530 v0 v1 v2 = du530 v2
du530 :: T494 -> AgdaAny -> AgdaAny
du530 v0
  = let v1 = d514 (coe v0) in
    let v2 = d516 (coe v0) in
    let v3 = d518 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du356
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe v3))))
name532 = "Algebra.IdempotentCommutativeMonoid._.identityˡ"
d532 :: T494 -> AgdaAny -> AgdaAny
d532 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d328
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))
name534
  = "Algebra.IdempotentCommutativeMonoid._.isCommutativeMonoid"
d534 :: T494 -> MAlonzo.Code.Algebra.Structures.T314
d534 v0
  = coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))
name536 = "Algebra.IdempotentCommutativeMonoid._.isEquivalence"
d536 :: T494 -> MAlonzo.Code.Relation.Binary.Core.T402
d536 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))))
name538 = "Algebra.IdempotentCommutativeMonoid._.isMagma"
d538 :: T494 -> MAlonzo.Code.Algebra.Structures.T84
d538 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))
name540 = "Algebra.IdempotentCommutativeMonoid._.isMonoid"
d540 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> MAlonzo.Code.Algebra.Structures.T268
d540 v0 v1 v2 = du540 v2
du540 :: T494 -> MAlonzo.Code.Algebra.Structures.T268
du540 v0
  = let v1 = d514 (coe v0) in
    let v2 = d516 (coe v0) in
    let v3 = d518 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe v3))))
name542 = "Algebra.IdempotentCommutativeMonoid._.isSemigroup"
d542 :: T494 -> MAlonzo.Code.Algebra.Structures.T118
d542 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))
name544 = "Algebra.IdempotentCommutativeMonoid._.refl"
d544 :: T494 -> AgdaAny -> AgdaAny
d544 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d376
                                 (coe (d518 (coe v0))))))))))))
name546 = "Algebra.IdempotentCommutativeMonoid._.reflexive"
d546 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d546 v0 v1 v2 = du546 v2
du546 ::
  T494 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du546 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d126 (coe v3) in
    coe
      (\ v5 v6 v7 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v4))) v5)
name548 = "Algebra.IdempotentCommutativeMonoid._.setoid"
d548 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> MAlonzo.Code.Relation.Binary.T128
d548 v0 v1 v2 = du548 v2
du548 :: T494 -> MAlonzo.Code.Relation.Binary.T128
du548 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name550 = "Algebra.IdempotentCommutativeMonoid._.sym"
d550 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d550 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d376
                                 (coe (d518 (coe v0))))))))))))
name552 = "Algebra.IdempotentCommutativeMonoid._.trans"
d552 ::
  T494 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d552 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d376
                                 (coe (d518 (coe v0))))))))))))
name554 = "Algebra.IdempotentCommutativeMonoid._.∙-cong"
d554 ::
  T494 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d554 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))))
name556 = "Algebra.IdempotentCommutativeMonoid._.∙-congʳ"
d556 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d556 v0 v1 v2 = du556 v2
du556 ::
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du556 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name558 = "Algebra.IdempotentCommutativeMonoid._.∙-congˡ"
d558 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d558 v0 v1 v2 = du558 v2
du558 ::
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du558 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name560 = "Algebra.IdempotentCommutativeMonoid.commutativeMonoid"
d560 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T416
d560 v0 v1 v2 = du560 v2
du560 :: T494 -> T416
du560 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1695 v3 v4 v5) erased erased (d514 (coe v0))
      (d516 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))
name564 = "Algebra.IdempotentCommutativeMonoid._.magma"
d564 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T28
d564 v0 v1 v2 = du564 v2
du564 :: T494 -> T28
du564 v0
  = let v1 = du560 (coe v0) in
    let v2 = du478 (coe v1) in coe (du122 (coe (du402 (coe v2))))
name566 = "Algebra.IdempotentCommutativeMonoid._.monoid"
d566 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T344
d566 v0 v1 v2 = du566 v2
du566 :: T494 -> T344
du566 v0 = coe (du478 (coe (du560 (coe v0))))
name568 = "Algebra.IdempotentCommutativeMonoid._.rawMagma"
d568 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T6
d568 v0 v1 v2 = du568 v2
du568 :: T494 -> T6
du568 v0
  = let v1 = du560 (coe v0) in
    let v2 = du478 (coe v1) in
    let v3 = du402 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name570 = "Algebra.IdempotentCommutativeMonoid._.rawMonoid"
d570 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T318
d570 v0 v1 v2 = du570 v2
du570 :: T494 -> T318
du570 v0
  = let v1 = du560 (coe v0) in coe (du404 (coe (du478 (coe v1))))
name572 = "Algebra.IdempotentCommutativeMonoid._.semigroup"
d572 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T76
d572 v0 v1 v2 = du572 v2
du572 :: T494 -> T76
du572 v0
  = let v1 = du560 (coe v0) in coe (du402 (coe (du478 (coe v1))))
name574 = "Algebra.BoundedLattice"
d574 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> ()
d574 = erased
name584 = "Algebra.BoundedLattice._∙_"
d584 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny
d584 v0 = coe (d514 (coe v0))
name586 = "Algebra.BoundedLattice._≈_"
d586 :: T494 -> AgdaAny -> AgdaAny -> ()
d586 = erased
name588 = "Algebra.BoundedLattice.Carrier"
d588 :: T494 -> ()
d588 = erased
name590 = "Algebra.BoundedLattice.assoc"
d590 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d590 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))
name592 = "Algebra.BoundedLattice.comm"
d592 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny
d592 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))
name594 = "Algebra.BoundedLattice.commutativeMonoid"
d594 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T416
d594 v0 v1 v2 = du594 v2
du594 :: T494 -> T416
du594 v0 = coe (du560 (coe v0))
name596 = "Algebra.BoundedLattice.idem"
d596 :: T494 -> AgdaAny -> AgdaAny
d596 v0
  = coe (MAlonzo.Code.Algebra.Structures.d378 (coe (d518 (coe v0))))
name598 = "Algebra.BoundedLattice.identity"
d598 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d598 v0 v1 v2 = du598 v2
du598 :: T494 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du598 v0
  = let v1 = d514 (coe v0) in
    let v2 = d516 (coe v0) in
    let v3 = d518 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe v3))))
name600 = "Algebra.BoundedLattice.identityʳ"
d600 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> AgdaAny -> AgdaAny
d600 v0 v1 v2 = du600 v2
du600 :: T494 -> AgdaAny -> AgdaAny
du600 v0
  = let v1 = d514 (coe v0) in
    let v2 = d516 (coe v0) in
    let v3 = d518 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du356
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe v3))))
name602 = "Algebra.BoundedLattice.identityˡ"
d602 :: T494 -> AgdaAny -> AgdaAny
d602 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d328
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))
name604 = "Algebra.BoundedLattice.isCommutativeMonoid"
d604 :: T494 -> MAlonzo.Code.Algebra.Structures.T314
d604 v0
  = coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))
name606 = "Algebra.BoundedLattice.isEquivalence"
d606 :: T494 -> MAlonzo.Code.Relation.Binary.Core.T402
d606 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))))
name608 = "Algebra.BoundedLattice.isIdempotentCommutativeMonoid"
d608 :: T494 -> MAlonzo.Code.Algebra.Structures.T366
d608 v0 = coe (d518 (coe v0))
name610 = "Algebra.BoundedLattice.isMagma"
d610 :: T494 -> MAlonzo.Code.Algebra.Structures.T84
d610 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))
name612 = "Algebra.BoundedLattice.isMonoid"
d612 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> MAlonzo.Code.Algebra.Structures.T268
d612 v0 v1 v2 = du612 v2
du612 :: T494 -> MAlonzo.Code.Algebra.Structures.T268
du612 v0
  = let v1 = d514 (coe v0) in
    let v2 = d516 (coe v0) in
    let v3 = d518 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe v3))))
name614 = "Algebra.BoundedLattice.isSemigroup"
d614 :: T494 -> MAlonzo.Code.Algebra.Structures.T118
d614 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))
name616 = "Algebra.BoundedLattice.magma"
d616 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T28
d616 v0 v1 v2 = du616 v2
du616 :: T494 -> T28
du616 v0
  = let v1 = du560 (coe v0) in
    let v2 = du478 (coe v1) in coe (du122 (coe (du402 (coe v2))))
name618 = "Algebra.BoundedLattice.monoid"
d618 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T344
d618 v0 v1 v2 = du618 v2
du618 :: T494 -> T344
du618 v0 = coe (du478 (coe (du560 (coe v0))))
name620 = "Algebra.BoundedLattice.rawMagma"
d620 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T6
d620 v0 v1 v2 = du620 v2
du620 :: T494 -> T6
du620 v0
  = let v1 = du560 (coe v0) in
    let v2 = du478 (coe v1) in
    let v3 = du402 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name622 = "Algebra.BoundedLattice.rawMonoid"
d622 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T318
d622 v0 v1 v2 = du622 v2
du622 :: T494 -> T318
du622 v0
  = let v1 = du560 (coe v0) in coe (du404 (coe (du478 (coe v1))))
name624 = "Algebra.BoundedLattice.refl"
d624 :: T494 -> AgdaAny -> AgdaAny
d624 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d376
                                 (coe (d518 (coe v0))))))))))))
name626 = "Algebra.BoundedLattice.reflexive"
d626 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d626 v0 v1 v2 = du626 v2
du626 ::
  T494 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du626 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d126 (coe v3) in
    coe
      (\ v5 v6 v7 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v4))) v5)
name628 = "Algebra.BoundedLattice.semigroup"
d628 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T494 -> T76
d628 v0 v1 v2 = du628 v2
du628 :: T494 -> T76
du628 v0
  = let v1 = du560 (coe v0) in coe (du402 (coe (du478 (coe v1))))
name630 = "Algebra.BoundedLattice.setoid"
d630 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> MAlonzo.Code.Relation.Binary.T128
d630 v0 v1 v2 = du630 v2
du630 :: T494 -> MAlonzo.Code.Relation.Binary.T128
du630 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name632 = "Algebra.BoundedLattice.sym"
d632 :: T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d632 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d376
                                 (coe (d518 (coe v0))))))))))))
name634 = "Algebra.BoundedLattice.trans"
d634 ::
  T494 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d634 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d376
                                 (coe (d518 (coe v0))))))))))))
name636 = "Algebra.BoundedLattice.ε"
d636 :: T494 -> AgdaAny
d636 v0 = coe (d516 (coe v0))
name638 = "Algebra.BoundedLattice.∙-cong"
d638 ::
  T494 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d638 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d376 (coe (d518 (coe v0))))))))))
name640 = "Algebra.BoundedLattice.∙-congʳ"
d640 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d640 v0 v1 v2 = du640 v2
du640 ::
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du640 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name642 = "Algebra.BoundedLattice.∙-congˡ"
d642 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d642 v0 v1 v2 = du642 v2
du642 ::
  T494 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du642 v0
  = let v1 = d518 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d376 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name648 = "Algebra.RawGroup"
d648 a0 a1 = ()
data T648
  = C2411 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          (AgdaAny -> AgdaAny)
name664 = "Algebra.RawGroup.Carrier"
d664 :: T648 -> ()
d664 = erased
name666 = "Algebra.RawGroup._≈_"
d666 :: T648 -> AgdaAny -> AgdaAny -> ()
d666 = erased
name668 = "Algebra.RawGroup._∙_"
d668 :: T648 -> AgdaAny -> AgdaAny -> AgdaAny
d668 v0
  = case coe v0 of
      C2411 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name670 = "Algebra.RawGroup.ε"
d670 :: T648 -> AgdaAny
d670 v0
  = case coe v0 of
      C2411 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name672 = "Algebra.RawGroup._⁻¹"
d672 :: T648 -> AgdaAny -> AgdaAny
d672 v0
  = case coe v0 of
      C2411 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name678 = "Algebra.Group"
d678 a0 a1 = ()
data T678
  = C2517 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          (AgdaAny -> AgdaAny) MAlonzo.Code.Algebra.Structures.T470
name696 = "Algebra.Group.Carrier"
d696 :: T678 -> ()
d696 = erased
name698 = "Algebra.Group._≈_"
d698 :: T678 -> AgdaAny -> AgdaAny -> ()
d698 = erased
name700 = "Algebra.Group._∙_"
d700 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny
d700 v0
  = case coe v0 of
      C2517 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name702 = "Algebra.Group.ε"
d702 :: T678 -> AgdaAny
d702 v0
  = case coe v0 of
      C2517 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name704 = "Algebra.Group._⁻¹"
d704 :: T678 -> AgdaAny -> AgdaAny
d704 v0
  = case coe v0 of
      C2517 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name706 = "Algebra.Group.isGroup"
d706 :: T678 -> MAlonzo.Code.Algebra.Structures.T470
d706 v0
  = case coe v0 of
      C2517 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name710 = "Algebra.Group._._-_"
d710 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 -> AgdaAny -> AgdaAny -> AgdaAny
d710 v0 v1 v2 = du710 v2
du710 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny
du710 v0
  = let v1 = d700 (coe v0) in
    let v2 = d704 (coe v0) in coe (\ v3 v4 -> coe v1 v3 (coe v2 v4))
name712 = "Algebra.Group._.assoc"
d712 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d712 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))))))
name714 = "Algebra.Group._.identity"
d714 :: T678 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d714 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))))
name716 = "Algebra.Group._.identityʳ"
d716 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> AgdaAny -> AgdaAny
d716 v0 v1 v2 = du716 v2
du716 :: T678 -> AgdaAny -> AgdaAny
du716 v0
  = let v1 = d706 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d484 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name718 = "Algebra.Group._.identityˡ"
d718 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> AgdaAny -> AgdaAny
d718 v0 v1 v2 = du718 v2
du718 :: T678 -> AgdaAny -> AgdaAny
du718 v0
  = let v1 = d706 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d484 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name720 = "Algebra.Group._.inverse"
d720 :: T678 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d720 v0
  = coe (MAlonzo.Code.Algebra.Structures.d486 (coe (d706 (coe v0))))
name722 = "Algebra.Group._.inverseʳ"
d722 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> AgdaAny -> AgdaAny
d722 v0 v1 v2 = du722 v2
du722 :: T678 -> AgdaAny -> AgdaAny
du722 v0
  = let v1 = d706 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v1))))
name724 = "Algebra.Group._.inverseˡ"
d724 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> AgdaAny -> AgdaAny
d724 v0 v1 v2 = du724 v2
du724 :: T678 -> AgdaAny -> AgdaAny
du724 v0
  = let v1 = d706 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v1))))
name726 = "Algebra.Group._.isEquivalence"
d726 :: T678 -> MAlonzo.Code.Relation.Binary.Core.T402
d726 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))))))))
name728 = "Algebra.Group._.isMagma"
d728 :: T678 -> MAlonzo.Code.Algebra.Structures.T84
d728 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))))))
name730 = "Algebra.Group._.isMonoid"
d730 :: T678 -> MAlonzo.Code.Algebra.Structures.T268
d730 v0
  = coe (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))
name732 = "Algebra.Group._.isSemigroup"
d732 :: T678 -> MAlonzo.Code.Algebra.Structures.T118
d732 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))))
name734 = "Algebra.Group._.refl"
d734 :: T678 -> AgdaAny -> AgdaAny
d734 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe (d706 (coe v0))))))))))))
name736 = "Algebra.Group._.reflexive"
d736 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d736 v0 v1 v2 = du736 v2
du736 ::
  T678 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du736 v0
  = let v1 = d706 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d484 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d126 (coe v3) in
    coe
      (\ v5 v6 v7 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v4))) v5)
name738 = "Algebra.Group._.setoid"
d738 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 -> MAlonzo.Code.Relation.Binary.T128
d738 v0 v1 v2 = du738 v2
du738 :: T678 -> MAlonzo.Code.Relation.Binary.T128
du738 v0
  = let v1 = d706 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d484 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name740 = "Algebra.Group._.sym"
d740 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d740 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe (d706 (coe v0))))))))))))
name742 = "Algebra.Group._.trans"
d742 ::
  T678 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d742 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe (d706 (coe v0))))))))))))
name744 = "Algebra.Group._.uniqueʳ-⁻¹"
d744 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d744 v0 v1 v2 = du744 v2
du744 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du744 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du542
         (coe (d700 (coe v0))) (coe (d702 (coe v0))) (coe (d704 (coe v0)))
         (coe (d706 (coe v0))))
name746 = "Algebra.Group._.uniqueˡ-⁻¹"
d746 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d746 v0 v1 v2 = du746 v2
du746 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du746 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du536
         (coe (d700 (coe v0))) (coe (d702 (coe v0))) (coe (d704 (coe v0)))
         (coe (d706 (coe v0))))
name748 = "Algebra.Group._.⁻¹-cong"
d748 :: T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d748 v0
  = coe (MAlonzo.Code.Algebra.Structures.d488 (coe (d706 (coe v0))))
name750 = "Algebra.Group._.∙-cong"
d750 ::
  T678 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d750 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))))))))
name752 = "Algebra.Group._.∙-congʳ"
d752 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d752 v0 v1 v2 = du752 v2
du752 ::
  T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du752 v0
  = let v1 = d706 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d484 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name754 = "Algebra.Group._.∙-congˡ"
d754 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d754 v0 v1 v2 = du754 v2
du754 ::
  T678 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du754 v0
  = let v1 = d706 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d484 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name756 = "Algebra.Group.rawGroup"
d756 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> T648
d756 v0 v1 v2 = du756 v2
du756 :: T678 -> T648
du756 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C2411 v3 v4 v5) erased erased (d700 (coe v0))
      (d702 (coe v0)) (d704 (coe v0))
name758 = "Algebra.Group.monoid"
d758 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> T344
d758 v0 v1 v2 = du758 v2
du758 :: T678 -> T344
du758 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1365 v3 v4 v5) erased erased (d700 (coe v0))
      (d702 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d484 (coe (d706 (coe v0))))
name762 = "Algebra.Group._.magma"
d762 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> T28
d762 v0 v1 v2 = du762 v2
du762 :: T678 -> T28
du762 v0
  = let v1 = du758 (coe v0) in coe (du122 (coe (du402 (coe v1))))
name764 = "Algebra.Group._.rawMagma"
d764 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> T6
d764 v0 v1 v2 = du764 v2
du764 :: T678 -> T6
du764 v0
  = let v1 = du758 (coe v0) in
    let v2 = du402 (coe v1) in coe (du70 (coe (du122 (coe v2))))
name766 = "Algebra.Group._.rawMonoid"
d766 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> T318
d766 v0 v1 v2 = du766 v2
du766 :: T678 -> T318
du766 v0 = coe (du404 (coe (du758 (coe v0))))
name768 = "Algebra.Group._.semigroup"
d768 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T678 -> T76
d768 v0 v1 v2 = du768 v2
du768 :: T678 -> T76
du768 v0 = coe (du402 (coe (du758 (coe v0))))
name774 = "Algebra.AbelianGroup"
d774 a0 a1 = ()
data T774
  = C2963 (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          (AgdaAny -> AgdaAny) MAlonzo.Code.Algebra.Structures.T550
name792 = "Algebra.AbelianGroup.Carrier"
d792 :: T774 -> ()
d792 = erased
name794 = "Algebra.AbelianGroup._≈_"
d794 :: T774 -> AgdaAny -> AgdaAny -> ()
d794 = erased
name796 = "Algebra.AbelianGroup._∙_"
d796 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny
d796 v0
  = case coe v0 of
      C2963 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name798 = "Algebra.AbelianGroup.ε"
d798 :: T774 -> AgdaAny
d798 v0
  = case coe v0 of
      C2963 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name800 = "Algebra.AbelianGroup._⁻¹"
d800 :: T774 -> AgdaAny -> AgdaAny
d800 v0
  = case coe v0 of
      C2963 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name802 = "Algebra.AbelianGroup.isAbelianGroup"
d802 :: T774 -> MAlonzo.Code.Algebra.Structures.T550
d802 v0
  = case coe v0 of
      C2963 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name806 = "Algebra.AbelianGroup._._-_"
d806 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> AgdaAny -> AgdaAny -> AgdaAny
d806 v0 v1 v2 = du806 v2
du806 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny
du806 v0
  = let v1 = d796 (coe v0) in
    let v2 = d800 (coe v0) in coe (\ v3 v4 -> coe v1 v3 (coe v2 v4))
name808 = "Algebra.AbelianGroup._.assoc"
d808 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d808 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))))))
name810 = "Algebra.AbelianGroup._.comm"
d810 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny
d810 v0
  = coe (MAlonzo.Code.Algebra.Structures.d564 (coe (d802 (coe v0))))
name812 = "Algebra.AbelianGroup._.identity"
d812 :: T774 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d812 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d484
               (coe
                  (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))))
name814 = "Algebra.AbelianGroup._.identityʳ"
d814 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> AgdaAny -> AgdaAny
d814 v0 v1 v2 = du814 v2
du814 :: T774 -> AgdaAny -> AgdaAny
du814 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d484 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v3))))
name816 = "Algebra.AbelianGroup._.identityˡ"
d816 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> AgdaAny -> AgdaAny
d816 v0 v1 v2 = du816 v2
du816 :: T774 -> AgdaAny -> AgdaAny
du816 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d484 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v3))))
name818 = "Algebra.AbelianGroup._.inverse"
d818 :: T774 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d818 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d486
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))
name820 = "Algebra.AbelianGroup._.inverseʳ"
d820 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> AgdaAny -> AgdaAny
d820 v0 v1 v2 = du820 v2
du820 :: T774 -> AgdaAny -> AgdaAny
du820 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v2))))
name822 = "Algebra.AbelianGroup._.inverseˡ"
d822 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> AgdaAny -> AgdaAny
d822 v0 v1 v2 = du822 v2
du822 :: T774 -> AgdaAny -> AgdaAny
du822 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v2))))
name824 = "Algebra.AbelianGroup._.isCommutativeMonoid"
d824 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> MAlonzo.Code.Algebra.Structures.T314
d824 v0 v1 v2 = du824 v2
du824 :: T774 -> MAlonzo.Code.Algebra.Structures.T314
du824 v0
  = coe (MAlonzo.Code.Algebra.Structures.du614 (coe (d802 (coe v0))))
name826 = "Algebra.AbelianGroup._.isEquivalence"
d826 :: T774 -> MAlonzo.Code.Relation.Binary.Core.T402
d826 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d562
                                 (coe (d802 (coe v0))))))))))))
name828 = "Algebra.AbelianGroup._.isGroup"
d828 :: T774 -> MAlonzo.Code.Algebra.Structures.T470
d828 v0
  = coe (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))
name830 = "Algebra.AbelianGroup._.isMagma"
d830 :: T774 -> MAlonzo.Code.Algebra.Structures.T84
d830 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))))))
name832 = "Algebra.AbelianGroup._.isMonoid"
d832 :: T774 -> MAlonzo.Code.Algebra.Structures.T268
d832 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d484
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))
name834 = "Algebra.AbelianGroup._.isSemigroup"
d834 :: T774 -> MAlonzo.Code.Algebra.Structures.T118
d834 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d484
               (coe
                  (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))))
name836 = "Algebra.AbelianGroup._.refl"
d836 :: T774 -> AgdaAny -> AgdaAny
d836 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe (d802 (coe v0))))))))))))))
name838 = "Algebra.AbelianGroup._.reflexive"
d838 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d838 v0 v1 v2 = du838 v2
du838 ::
  T774 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du838 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d484 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d126 (coe v4) in
    coe
      (\ v6 v7 v8 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v5))) v6)
name840 = "Algebra.AbelianGroup._.setoid"
d840 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> MAlonzo.Code.Relation.Binary.T128
d840 v0 v1 v2 = du840 v2
du840 :: T774 -> MAlonzo.Code.Relation.Binary.T128
du840 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d484 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name842 = "Algebra.AbelianGroup._.sym"
d842 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d842 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe (d802 (coe v0))))))))))))))
name844 = "Algebra.AbelianGroup._.trans"
d844 ::
  T774 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d844 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe (d802 (coe v0))))))))))))))
name846 = "Algebra.AbelianGroup._.uniqueʳ-⁻¹"
d846 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d846 v0 v1 v2 = du846 v2
du846 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du846 v0
  = let v1 = d796 (coe v0) in
    let v2 = d798 (coe v0) in
    let v3 = d800 (coe v0) in
    let v4 = d802 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du542
         (coe v1) (coe v2) (coe v3)
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe v4))))
name848 = "Algebra.AbelianGroup._.uniqueˡ-⁻¹"
d848 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d848 v0 v1 v2 = du848 v2
du848 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du848 v0
  = let v1 = d796 (coe v0) in
    let v2 = d798 (coe v0) in
    let v3 = d800 (coe v0) in
    let v4 = d802 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du536
         (coe v1) (coe v2) (coe v3)
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe v4))))
name850 = "Algebra.AbelianGroup._.⁻¹-cong"
d850 :: T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d850 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d488
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))))
name852 = "Algebra.AbelianGroup._.∙-cong"
d852 ::
  T774 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d852 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d562
                                 (coe (d802 (coe v0))))))))))))
name854 = "Algebra.AbelianGroup._.∙-congʳ"
d854 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d854 v0 v1 v2 = du854 v2
du854 ::
  T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du854 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d484 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name856 = "Algebra.AbelianGroup._.∙-congˡ"
d856 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d856 v0 v1 v2 = du856 v2
du856 ::
  T774 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du856 v0
  = let v1 = d802 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d562 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d484 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name858 = "Algebra.AbelianGroup.group"
d858 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T678
d858 v0 v1 v2 = du858 v2
du858 :: T774 -> T678
du858 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C2517 v3 v4 v5 v6) erased erased
      (d796 (coe v0)) (d798 (coe v0)) (d800 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d562 (coe (d802 (coe v0))))
name862 = "Algebra.AbelianGroup._.magma"
d862 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T28
d862 v0 v1 v2 = du862 v2
du862 :: T774 -> T28
du862 v0
  = let v1 = du858 (coe v0) in
    let v2 = du758 (coe v1) in coe (du122 (coe (du402 (coe v2))))
name864 = "Algebra.AbelianGroup._.monoid"
d864 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T344
d864 v0 v1 v2 = du864 v2
du864 :: T774 -> T344
du864 v0 = coe (du758 (coe (du858 (coe v0))))
name866 = "Algebra.AbelianGroup._.rawGroup"
d866 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T648
d866 v0 v1 v2 = du866 v2
du866 :: T774 -> T648
du866 v0 = coe (du756 (coe (du858 (coe v0))))
name868 = "Algebra.AbelianGroup._.rawMagma"
d868 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T6
d868 v0 v1 v2 = du868 v2
du868 :: T774 -> T6
du868 v0
  = let v1 = du858 (coe v0) in
    let v2 = du758 (coe v1) in
    let v3 = du402 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name870 = "Algebra.AbelianGroup._.rawMonoid"
d870 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T318
d870 v0 v1 v2 = du870 v2
du870 :: T774 -> T318
du870 v0
  = let v1 = du858 (coe v0) in coe (du404 (coe (du758 (coe v1))))
name872 = "Algebra.AbelianGroup._.semigroup"
d872 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T76
d872 v0 v1 v2 = du872 v2
du872 :: T774 -> T76
du872 v0
  = let v1 = du858 (coe v0) in coe (du402 (coe (du758 (coe v1))))
name874 = "Algebra.AbelianGroup.commutativeMonoid"
d874 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T774 -> T416
d874 v0 v1 v2 = du874 v2
du874 :: T774 -> T416
du874 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1695 v3 v4 v5) erased erased (d796 (coe v0))
      (d798 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du614 (coe (d802 (coe v0))))
name880 = "Algebra.Lattice"
d880 a0 a1 = ()
data T880
  = C3441 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny)
          MAlonzo.Code.Algebra.Structures.T620
name896 = "Algebra.Lattice.Carrier"
d896 :: T880 -> ()
d896 = erased
name898 = "Algebra.Lattice._≈_"
d898 :: T880 -> AgdaAny -> AgdaAny -> ()
d898 = erased
name900 = "Algebra.Lattice._∨_"
d900 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny
d900 v0
  = case coe v0 of
      C3441 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name902 = "Algebra.Lattice._∧_"
d902 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny
d902 v0
  = case coe v0 of
      C3441 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name904 = "Algebra.Lattice.isLattice"
d904 :: T880 -> MAlonzo.Code.Algebra.Structures.T620
d904 v0
  = case coe v0 of
      C3441 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name908 = "Algebra.Lattice._.absorptive"
d908 :: T880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d908 v0
  = coe (MAlonzo.Code.Algebra.Structures.d656 (coe (d904 (coe v0))))
name910 = "Algebra.Lattice._.isEquivalence"
d910 :: T880 -> MAlonzo.Code.Relation.Binary.Core.T402
d910 v0
  = coe (MAlonzo.Code.Algebra.Structures.d642 (coe (d904 (coe v0))))
name912 = "Algebra.Lattice._.refl"
d912 :: T880 -> AgdaAny -> AgdaAny
d912 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe (MAlonzo.Code.Algebra.Structures.d642 (coe (d904 (coe v0))))))
name914 = "Algebra.Lattice._.reflexive"
d914 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d914 v0 v1 v2 = du914 v2
du914 ::
  T880 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du914 v0
  = let v1 = d904 (coe v0) in
    coe
      (\ v2 v3 v4 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d642 (coe v1))) v2)
name916 = "Algebra.Lattice._.sym"
d916 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d916 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe (MAlonzo.Code.Algebra.Structures.d642 (coe (d904 (coe v0))))))
name918 = "Algebra.Lattice._.trans"
d918 ::
  T880 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d918 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe (MAlonzo.Code.Algebra.Structures.d642 (coe (d904 (coe v0))))))
name920 = "Algebra.Lattice._.∧-absorbs-∨"
d920 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> AgdaAny -> AgdaAny -> AgdaAny
d920 v0 v1 v2 = du920 v2
du920 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny
du920 v0
  = let v1 = d904 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d656 (coe v1))))
name922 = "Algebra.Lattice._.∧-assoc"
d922 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d922 v0
  = coe (MAlonzo.Code.Algebra.Structures.d652 (coe (d904 (coe v0))))
name924 = "Algebra.Lattice._.∧-comm"
d924 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny
d924 v0
  = coe (MAlonzo.Code.Algebra.Structures.d650 (coe (d904 (coe v0))))
name926 = "Algebra.Lattice._.∧-cong"
d926 ::
  T880 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d926 v0
  = coe (MAlonzo.Code.Algebra.Structures.d654 (coe (d904 (coe v0))))
name928 = "Algebra.Lattice._.∧-congʳ"
d928 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d928 v0 v1 v2 = du928 v2
du928 ::
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du928 v0
  = coe (MAlonzo.Code.Algebra.Structures.du676 (coe (d904 (coe v0))))
name930 = "Algebra.Lattice._.∧-congˡ"
d930 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d930 v0 v1 v2 = du930 v2
du930 ::
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du930 v0
  = coe (MAlonzo.Code.Algebra.Structures.du672 (coe (d904 (coe v0))))
name932 = "Algebra.Lattice._.∨-absorbs-∧"
d932 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> AgdaAny -> AgdaAny -> AgdaAny
d932 v0 v1 v2 = du932 v2
du932 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny
du932 v0
  = let v1 = d904 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d656 (coe v1))))
name934 = "Algebra.Lattice._.∨-assoc"
d934 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d934 v0
  = coe (MAlonzo.Code.Algebra.Structures.d646 (coe (d904 (coe v0))))
name936 = "Algebra.Lattice._.∨-comm"
d936 :: T880 -> AgdaAny -> AgdaAny -> AgdaAny
d936 v0
  = coe (MAlonzo.Code.Algebra.Structures.d644 (coe (d904 (coe v0))))
name938 = "Algebra.Lattice._.∨-cong"
d938 ::
  T880 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d938 v0
  = coe (MAlonzo.Code.Algebra.Structures.d648 (coe (d904 (coe v0))))
name940 = "Algebra.Lattice._.∨-congʳ"
d940 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d940 v0 v1 v2 = du940 v2
du940 ::
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du940 v0
  = coe (MAlonzo.Code.Algebra.Structures.du684 (coe (d904 (coe v0))))
name942 = "Algebra.Lattice._.∨-congˡ"
d942 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d942 v0 v1 v2 = du942 v2
du942 ::
  T880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du942 v0
  = coe (MAlonzo.Code.Algebra.Structures.du680 (coe (d904 (coe v0))))
name944 = "Algebra.Lattice.setoid"
d944 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T880 -> MAlonzo.Code.Relation.Binary.T128
d944 v0 v1 v2 = du944 v2
du944 :: T880 -> MAlonzo.Code.Relation.Binary.T128
du944 v0
  = coe
      (\ v1 v2 v3 -> MAlonzo.Code.Relation.Binary.C1037 v3) erased erased
      (MAlonzo.Code.Algebra.Structures.d642 (coe (d904 (coe v0))))
name950 = "Algebra.DistributiveLattice"
d950 a0 a1 = ()
data T950
  = C3739 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny)
          MAlonzo.Code.Algebra.Structures.T692
name966 = "Algebra.DistributiveLattice.Carrier"
d966 :: T950 -> ()
d966 = erased
name968 = "Algebra.DistributiveLattice._≈_"
d968 :: T950 -> AgdaAny -> AgdaAny -> ()
d968 = erased
name970 = "Algebra.DistributiveLattice._∨_"
d970 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny
d970 v0
  = case coe v0 of
      C3739 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name972 = "Algebra.DistributiveLattice._∧_"
d972 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny
d972 v0
  = case coe v0 of
      C3739 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name974 = "Algebra.DistributiveLattice.isDistributiveLattice"
d974 :: T950 -> MAlonzo.Code.Algebra.Structures.T692
d974 v0
  = case coe v0 of
      C3739 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name978 = "Algebra.DistributiveLattice._.absorptive"
d978 :: T950 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d978 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d656
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name980 = "Algebra.DistributiveLattice._.isEquivalence"
d980 :: T950 -> MAlonzo.Code.Relation.Binary.Core.T402
d980 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d642
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name982 = "Algebra.DistributiveLattice._.isLattice"
d982 :: T950 -> MAlonzo.Code.Algebra.Structures.T620
d982 v0
  = coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))
name984 = "Algebra.DistributiveLattice._.refl"
d984 :: T950 -> AgdaAny -> AgdaAny
d984 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d642
               (coe
                  (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))))
name986 = "Algebra.DistributiveLattice._.reflexive"
d986 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d986 v0 v1 v2 = du986 v2
du986 ::
  T950 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du986 v0
  = let v1 = d974 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d702 (coe v1) in
    coe
      (\ v3 v4 v5 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d642 (coe v2))) v3)
name988 = "Algebra.DistributiveLattice._.sym"
d988 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d988 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d642
               (coe
                  (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))))
name990 = "Algebra.DistributiveLattice._.trans"
d990 ::
  T950 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d990 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d642
               (coe
                  (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))))
name992 = "Algebra.DistributiveLattice._.∧-absorbs-∨"
d992 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny
d992 v0 v1 v2 = du992 v2
du992 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny
du992 v0
  = let v1 = d974 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d702 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d656 (coe v2))))
name994 = "Algebra.DistributiveLattice._.∧-assoc"
d994 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d994 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d652
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name996 = "Algebra.DistributiveLattice._.∧-comm"
d996 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny
d996 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d650
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name998 = "Algebra.DistributiveLattice._.∧-cong"
d998 ::
  T950 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d998 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d654
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name1000 = "Algebra.DistributiveLattice._.∧-congʳ"
d1000 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1000 v0 v1 v2 = du1000 v2
du1000 ::
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1000 v0
  = let v1 = d974 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du676
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v1))))
name1002 = "Algebra.DistributiveLattice._.∧-congˡ"
d1002 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1002 v0 v1 v2 = du1002 v2
du1002 ::
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1002 v0
  = let v1 = d974 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du672
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v1))))
name1004 = "Algebra.DistributiveLattice._.∨-absorbs-∧"
d1004 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny
d1004 v0 v1 v2 = du1004 v2
du1004 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny
du1004 v0
  = let v1 = d974 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d702 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d656 (coe v2))))
name1006 = "Algebra.DistributiveLattice._.∨-assoc"
d1006 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1006 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d646
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name1008 = "Algebra.DistributiveLattice._.∨-comm"
d1008 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny
d1008 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d644
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name1010 = "Algebra.DistributiveLattice._.∨-cong"
d1010 ::
  T950 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1010 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d648
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))))
name1012 = "Algebra.DistributiveLattice._.∨-congʳ"
d1012 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1012 v0 v1 v2 = du1012 v2
du1012 ::
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1012 v0
  = let v1 = d974 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du684
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v1))))
name1014 = "Algebra.DistributiveLattice._.∨-congˡ"
d1014 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1014 v0 v1 v2 = du1014 v2
du1014 ::
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1014 v0
  = let v1 = d974 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du680
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v1))))
name1016 = "Algebra.DistributiveLattice._.∨-distribʳ-∧"
d1016 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1016 v0
  = coe (MAlonzo.Code.Algebra.Structures.d704 (coe (d974 (coe v0))))
name1018 = "Algebra.DistributiveLattice._.∨-∧-distribʳ"
d1018 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1018 v0 v1 v2 = du1018 v2
du1018 :: T950 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1018 v0
  = coe (MAlonzo.Code.Algebra.Structures.du744 (coe (d974 (coe v0))))
name1020 = "Algebra.DistributiveLattice.lattice"
d1020 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T950 -> T880
d1020 v0 v1 v2 = du1020 v2
du1020 :: T950 -> T880
du1020 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C3441 v3 v4 v5) erased erased (d970 (coe v0))
      (d972 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d702 (coe (d974 (coe v0))))
name1024 = "Algebra.DistributiveLattice._.setoid"
d1024 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T950 -> MAlonzo.Code.Relation.Binary.T128
d1024 v0 v1 v2 = du1024 v2
du1024 :: T950 -> MAlonzo.Code.Relation.Binary.T128
du1024 v0 = coe (du944 (coe (du1020 (coe v0))))
name1030 = "Algebra.NearSemiring"
d1030 a0 a1 = ()
data T1030
  = C4071 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          MAlonzo.Code.Algebra.Structures.T752
name1048 = "Algebra.NearSemiring.Carrier"
d1048 :: T1030 -> ()
d1048 = erased
name1050 = "Algebra.NearSemiring._≈_"
d1050 :: T1030 -> AgdaAny -> AgdaAny -> ()
d1050 = erased
name1052 = "Algebra.NearSemiring._+_"
d1052 :: T1030 -> AgdaAny -> AgdaAny -> AgdaAny
d1052 v0
  = case coe v0 of
      C4071 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1054 = "Algebra.NearSemiring._*_"
d1054 :: T1030 -> AgdaAny -> AgdaAny -> AgdaAny
d1054 v0
  = case coe v0 of
      C4071 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1056 = "Algebra.NearSemiring.0#"
d1056 :: T1030 -> AgdaAny
d1056 v0
  = case coe v0 of
      C4071 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1058 = "Algebra.NearSemiring.isNearSemiring"
d1058 :: T1030 -> MAlonzo.Code.Algebra.Structures.T752
d1058 v0
  = case coe v0 of
      C4071 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1062 = "Algebra.NearSemiring._.assoc"
d1062 :: T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1062 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d770 (coe (d1058 (coe v0))))))
name1064 = "Algebra.NearSemiring._.∙-cong"
d1064 ::
  T1030 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1064 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d770 (coe (d1058 (coe v0))))))))
name1066 = "Algebra.NearSemiring._.∙-congʳ"
d1066 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1066 v0 v1 v2 = du1066 v2
du1066 ::
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1066 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d770 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name1068 = "Algebra.NearSemiring._.∙-congˡ"
d1068 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1068 v0 v1 v2 = du1068 v2
du1068 ::
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1068 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d770 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v2))))
name1070 = "Algebra.NearSemiring._.isMagma"
d1070 :: T1030 -> MAlonzo.Code.Algebra.Structures.T84
d1070 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d770 (coe (d1058 (coe v0))))))
name1072 = "Algebra.NearSemiring._.*-isSemigroup"
d1072 :: T1030 -> MAlonzo.Code.Algebra.Structures.T118
d1072 v0
  = coe (MAlonzo.Code.Algebra.Structures.d770 (coe (d1058 (coe v0))))
name1074 = "Algebra.NearSemiring._.assoc"
d1074 :: T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1074 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))))))
name1076 = "Algebra.NearSemiring._.∙-cong"
d1076 ::
  T1030 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1076 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))))))))
name1078 = "Algebra.NearSemiring._.∙-congʳ"
d1078 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1078 v0 v1 v2 = du1078 v2
du1078 ::
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1078 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d768 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1080 = "Algebra.NearSemiring._.∙-congˡ"
d1080 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1080 v0 v1 v2 = du1080 v2
du1080 ::
  T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1080 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d768 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1082 = "Algebra.NearSemiring._.identity"
d1082 :: T1030 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1082 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))))
name1084 = "Algebra.NearSemiring._.identityʳ"
d1084 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> AgdaAny -> AgdaAny
d1084 v0 v1 v2 = du1084 v2
du1084 :: T1030 -> AgdaAny -> AgdaAny
du1084 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d768 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name1086 = "Algebra.NearSemiring._.identityˡ"
d1086 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> AgdaAny -> AgdaAny
d1086 v0 v1 v2 = du1086 v2
du1086 :: T1030 -> AgdaAny -> AgdaAny
du1086 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d768 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name1088 = "Algebra.NearSemiring._.isMagma"
d1088 :: T1030 -> MAlonzo.Code.Algebra.Structures.T84
d1088 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))))))
name1090 = "Algebra.NearSemiring._.+-isMonoid"
d1090 :: T1030 -> MAlonzo.Code.Algebra.Structures.T268
d1090 v0
  = coe (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))
name1092 = "Algebra.NearSemiring._.isSemigroup"
d1092 :: T1030 -> MAlonzo.Code.Algebra.Structures.T118
d1092 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))))
name1094 = "Algebra.NearSemiring._.distribʳ"
d1094 :: T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1094 v0
  = coe (MAlonzo.Code.Algebra.Structures.d772 (coe (d1058 (coe v0))))
name1096 = "Algebra.NearSemiring._.isEquivalence"
d1096 :: T1030 -> MAlonzo.Code.Relation.Binary.Core.T402
d1096 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))))))))
name1098 = "Algebra.NearSemiring._.refl"
d1098 :: T1030 -> AgdaAny -> AgdaAny
d1098 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d768
                                 (coe (d1058 (coe v0))))))))))))
name1100 = "Algebra.NearSemiring._.reflexive"
d1100 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1030 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1100 v0 v1 v2 = du1100 v2
du1100 ::
  T1030 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1100 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d768 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d126 (coe v3) in
    coe
      (\ v5 v6 v7 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v4))) v5)
name1102 = "Algebra.NearSemiring._.setoid"
d1102 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1030 -> MAlonzo.Code.Relation.Binary.T128
d1102 v0 v1 v2 = du1102 v2
du1102 :: T1030 -> MAlonzo.Code.Relation.Binary.T128
du1102 v0
  = let v1 = d1058 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d768 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1104 = "Algebra.NearSemiring._.sym"
d1104 :: T1030 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1104 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d768
                                 (coe (d1058 (coe v0))))))))))))
name1106 = "Algebra.NearSemiring._.trans"
d1106 ::
  T1030 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1106 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d768
                                 (coe (d1058 (coe v0))))))))))))
name1108 = "Algebra.NearSemiring._.zeroˡ"
d1108 :: T1030 -> AgdaAny -> AgdaAny
d1108 v0
  = coe (MAlonzo.Code.Algebra.Structures.d774 (coe (d1058 (coe v0))))
name1110 = "Algebra.NearSemiring.+-monoid"
d1110 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T344
d1110 v0 v1 v2 = du1110 v2
du1110 :: T1030 -> T344
du1110 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1365 v3 v4 v5) erased erased (d1052 (coe v0))
      (d1056 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d768 (coe (d1058 (coe v0))))
name1114 = "Algebra.NearSemiring._.magma"
d1114 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T28
d1114 v0 v1 v2 = du1114 v2
du1114 :: T1030 -> T28
du1114 v0
  = let v1 = du1110 (coe v0) in coe (du122 (coe (du402 (coe v1))))
name1116 = "Algebra.NearSemiring._.rawMagma"
d1116 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T6
d1116 v0 v1 v2 = du1116 v2
du1116 :: T1030 -> T6
du1116 v0
  = let v1 = du1110 (coe v0) in
    let v2 = du402 (coe v1) in coe (du70 (coe (du122 (coe v2))))
name1118 = "Algebra.NearSemiring._.rawMonoid"
d1118 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T318
d1118 v0 v1 v2 = du1118 v2
du1118 :: T1030 -> T318
du1118 v0 = coe (du404 (coe (du1110 (coe v0))))
name1120 = "Algebra.NearSemiring._.semigroup"
d1120 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T76
d1120 v0 v1 v2 = du1120 v2
du1120 :: T1030 -> T76
du1120 v0 = coe (du402 (coe (du1110 (coe v0))))
name1122 = "Algebra.NearSemiring.*-semigroup"
d1122 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T76
d1122 v0 v1 v2 = du1122 v2
du1122 :: T1030 -> T76
du1122 v0
  = coe
      (\ v1 v2 v3 v4 -> C285 v3 v4) erased erased (d1054 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d770 (coe (d1058 (coe v0))))
name1126 = "Algebra.NearSemiring._.magma"
d1126 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T28
d1126 v0 v1 v2 = du1126 v2
du1126 :: T1030 -> T28
du1126 v0 = coe (du122 (coe (du1122 (coe v0))))
name1128 = "Algebra.NearSemiring._.rawMagma"
d1128 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1030 -> T6
d1128 v0 v1 v2 = du1128 v2
du1128 :: T1030 -> T6
du1128 v0
  = let v1 = du1122 (coe v0) in coe (du70 (coe (du122 (coe v1))))
name1134 = "Algebra.SemiringWithoutOne"
d1134 a0 a1 = ()
data T1134
  = C4519 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          MAlonzo.Code.Algebra.Structures.T826
name1152 = "Algebra.SemiringWithoutOne.Carrier"
d1152 :: T1134 -> ()
d1152 = erased
name1154 = "Algebra.SemiringWithoutOne._≈_"
d1154 :: T1134 -> AgdaAny -> AgdaAny -> ()
d1154 = erased
name1156 = "Algebra.SemiringWithoutOne._+_"
d1156 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny
d1156 v0
  = case coe v0 of
      C4519 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1158 = "Algebra.SemiringWithoutOne._*_"
d1158 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny
d1158 v0
  = case coe v0 of
      C4519 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1160 = "Algebra.SemiringWithoutOne.0#"
d1160 :: T1134 -> AgdaAny
d1160 v0
  = case coe v0 of
      C4519 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1162 = "Algebra.SemiringWithoutOne.isSemiringWithoutOne"
d1162 :: T1134 -> MAlonzo.Code.Algebra.Structures.T826
d1162 v0
  = case coe v0 of
      C4519 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1166 = "Algebra.SemiringWithoutOne._.assoc"
d1166 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1166 v0 v1 v2 = du1166 v2
du1166 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1166 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe (MAlonzo.Code.Algebra.Structures.d844 (coe v1))))
name1168 = "Algebra.SemiringWithoutOne._.∙-cong"
d1168 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1168 v0 v1 v2 = du1168 v2
du1168 ::
  T1134 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1168 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe (MAlonzo.Code.Algebra.Structures.d844 (coe v1))))))
name1170 = "Algebra.SemiringWithoutOne._.∙-congʳ"
d1170 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1170 v0 v1 v2 = du1170 v2
du1170 ::
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1170 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d770 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v5))))
name1172 = "Algebra.SemiringWithoutOne._.∙-congˡ"
d1172 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1172 v0 v1 v2 = du1172 v2
du1172 ::
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1172 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d770 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v5))))
name1174 = "Algebra.SemiringWithoutOne._.isMagma"
d1174 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Algebra.Structures.T84
d1174 v0 v1 v2 = du1174 v2
du1174 :: T1134 -> MAlonzo.Code.Algebra.Structures.T84
du1174 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe (MAlonzo.Code.Algebra.Structures.d844 (coe v1))))
name1176 = "Algebra.SemiringWithoutOne._.*-isSemigroup"
d1176 :: T1134 -> MAlonzo.Code.Algebra.Structures.T118
d1176 v0
  = coe (MAlonzo.Code.Algebra.Structures.d844 (coe (d1162 (coe v0))))
name1178 = "Algebra.SemiringWithoutOne._.*-isSemigroup"
d1178 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Algebra.Structures.T118
d1178 v0 v1 v2 = du1178 v2
du1178 :: T1134 -> MAlonzo.Code.Algebra.Structures.T118
du1178 v0
  = let v1 = d1162 (coe v0) in
    coe (MAlonzo.Code.Algebra.Structures.d844 (coe v1))
name1180 = "Algebra.SemiringWithoutOne._.assoc"
d1180 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1180 v0 v1 v2 = du1180 v2
du1180 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1180 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))
name1182 = "Algebra.SemiringWithoutOne._.comm"
d1182 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny
d1182 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe
            (MAlonzo.Code.Algebra.Structures.d842 (coe (d1162 (coe v0))))))
name1184 = "Algebra.SemiringWithoutOne._.∙-cong"
d1184 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1184 v0 v1 v2 = du1184 v2
du1184 ::
  T1134 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1184 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))))
name1186 = "Algebra.SemiringWithoutOne._.∙-congʳ"
d1186 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1186 v0 v1 v2 = du1186 v2
du1186 ::
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1186 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d768 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name1188 = "Algebra.SemiringWithoutOne._.∙-congˡ"
d1188 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1188 v0 v1 v2 = du1188 v2
du1188 ::
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1188 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d768 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name1190 = "Algebra.SemiringWithoutOne._.identity"
d1190 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1190 v0 v1 v2 = du1190 v2
du1190 :: T1134 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1190 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v3))))
name1192 = "Algebra.SemiringWithoutOne._.identityʳ"
d1192 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> AgdaAny -> AgdaAny
d1192 v0 v1 v2 = du1192 v2
du1192 :: T1134 -> AgdaAny -> AgdaAny
du1192 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d768 (coe v4) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v5))))
name1194 = "Algebra.SemiringWithoutOne._.identityˡ"
d1194 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> AgdaAny -> AgdaAny
d1194 v0 v1 v2 = du1194 v2
du1194 :: T1134 -> AgdaAny -> AgdaAny
du1194 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d768 (coe v4) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v5))))
name1196 = "Algebra.SemiringWithoutOne._.+-isCommutativeMonoid"
d1196 :: T1134 -> MAlonzo.Code.Algebra.Structures.T314
d1196 v0
  = coe (MAlonzo.Code.Algebra.Structures.d842 (coe (d1162 (coe v0))))
name1198 = "Algebra.SemiringWithoutOne._.isMagma"
d1198 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Algebra.Structures.T84
d1198 v0 v1 v2 = du1198 v2
du1198 :: T1134 -> MAlonzo.Code.Algebra.Structures.T84
du1198 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))
name1200 = "Algebra.SemiringWithoutOne._.isMonoid"
d1200 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Algebra.Structures.T268
d1200 v0 v1 v2 = du1200 v2
du1200 :: T1134 -> MAlonzo.Code.Algebra.Structures.T268
du1200 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v3))))
name1202 = "Algebra.SemiringWithoutOne._.isSemigroup"
d1202 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Algebra.Structures.T118
d1202 v0 v1 v2 = du1202 v2
du1202 :: T1134 -> MAlonzo.Code.Algebra.Structures.T118
du1202 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))
name1204 = "Algebra.SemiringWithoutOne._.distrib"
d1204 :: T1134 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1204 v0
  = coe (MAlonzo.Code.Algebra.Structures.d846 (coe (d1162 (coe v0))))
name1206 = "Algebra.SemiringWithoutOne._.distribʳ"
d1206 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1206 v0 v1 v2 = du1206 v2
du1206 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1206 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d846 (coe v1))))
name1208 = "Algebra.SemiringWithoutOne._.isEquivalence"
d1208 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Relation.Binary.Core.T402
d1208 v0 v1 v2 = du1208 v2
du1208 :: T1134 -> MAlonzo.Code.Relation.Binary.Core.T402
du1208 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))))
name1210 = "Algebra.SemiringWithoutOne._.isNearSemiring"
d1210 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Algebra.Structures.T752
d1210 v0 v1 v2 = du1210 v2
du1210 :: T1134 -> MAlonzo.Code.Algebra.Structures.T752
du1210 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du860
         (coe (d1156 (coe v0))) (coe (d1160 (coe v0)))
         (coe (d1162 (coe v0))))
name1212 = "Algebra.SemiringWithoutOne._.refl"
d1212 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> AgdaAny -> AgdaAny
d1212 v0 v1 v2 = du1212 v2
du1212 :: T1134 -> AgdaAny -> AgdaAny
du1212 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))))))
name1214 = "Algebra.SemiringWithoutOne._.reflexive"
d1214 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1214 v0 v1 v2 = du1214 v2
du1214 ::
  T1134 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1214 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d768 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d126 (coe v6) in
    coe
      (\ v8 v9 v10 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v7))) v8)
name1216 = "Algebra.SemiringWithoutOne._.setoid"
d1216 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> MAlonzo.Code.Relation.Binary.T128
d1216 v0 v1 v2 = du1216 v2
du1216 :: T1134 -> MAlonzo.Code.Relation.Binary.T128
du1216 v0
  = let v1 = d1156 (coe v0) in
    let v2 = d1160 (coe v0) in
    let v3 = d1162 (coe v0) in
    let v4
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d768 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name1218 = "Algebra.SemiringWithoutOne._.sym"
d1218 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1218 v0 v1 v2 = du1218 v2
du1218 :: T1134 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1218 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))))))
name1220 = "Algebra.SemiringWithoutOne._.trans"
d1220 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1134 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1220 v0 v1 v2 = du1220 v2
du1220 ::
  T1134 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1220 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v1))))))))))
name1222 = "Algebra.SemiringWithoutOne._.zero"
d1222 :: T1134 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1222 v0
  = coe (MAlonzo.Code.Algebra.Structures.d848 (coe (d1162 (coe v0))))
name1224 = "Algebra.SemiringWithoutOne._.zeroʳ"
d1224 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> AgdaAny -> AgdaAny
d1224 v0 v1 v2 = du1224 v2
du1224 :: T1134 -> AgdaAny -> AgdaAny
du1224 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d848 (coe v1))))
name1226 = "Algebra.SemiringWithoutOne._.zeroˡ"
d1226 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> AgdaAny -> AgdaAny
d1226 v0 v1 v2 = du1226 v2
du1226 :: T1134 -> AgdaAny -> AgdaAny
du1226 v0
  = let v1 = d1162 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d848 (coe v1))))
name1228 = "Algebra.SemiringWithoutOne.nearSemiring"
d1228 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T1030
d1228 v0 v1 v2 = du1228 v2
du1228 :: T1134 -> T1030
du1228 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C4071 v3 v4 v5 v6) erased erased
      (d1156 (coe v0)) (d1158 (coe v0)) (d1160 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du860
         (coe (d1156 (coe v0))) (coe (d1160 (coe v0)))
         (coe (d1162 (coe v0))))
name1232 = "Algebra.SemiringWithoutOne._.magma"
d1232 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T28
d1232 v0 v1 v2 = du1232 v2
du1232 :: T1134 -> T28
du1232 v0
  = let v1 = du1228 (coe v0) in coe (du122 (coe (du1122 (coe v1))))
name1234 = "Algebra.SemiringWithoutOne._.rawMagma"
d1234 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T6
d1234 v0 v1 v2 = du1234 v2
du1234 :: T1134 -> T6
du1234 v0
  = let v1 = du1228 (coe v0) in
    let v2 = du1122 (coe v1) in coe (du70 (coe (du122 (coe v2))))
name1236 = "Algebra.SemiringWithoutOne._.*-semigroup"
d1236 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T76
d1236 v0 v1 v2 = du1236 v2
du1236 :: T1134 -> T76
du1236 v0 = coe (du1122 (coe (du1228 (coe v0))))
name1238 = "Algebra.SemiringWithoutOne._.magma"
d1238 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T28
d1238 v0 v1 v2 = du1238 v2
du1238 :: T1134 -> T28
du1238 v0
  = let v1 = du1228 (coe v0) in
    let v2 = du1110 (coe v1) in coe (du122 (coe (du402 (coe v2))))
name1240 = "Algebra.SemiringWithoutOne._.+-monoid"
d1240 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T344
d1240 v0 v1 v2 = du1240 v2
du1240 :: T1134 -> T344
du1240 v0 = coe (du1110 (coe (du1228 (coe v0))))
name1242 = "Algebra.SemiringWithoutOne._.rawMagma"
d1242 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T6
d1242 v0 v1 v2 = du1242 v2
du1242 :: T1134 -> T6
du1242 v0
  = let v1 = du1228 (coe v0) in
    let v2 = du1110 (coe v1) in
    let v3 = du402 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name1244 = "Algebra.SemiringWithoutOne._.rawMonoid"
d1244 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T318
d1244 v0 v1 v2 = du1244 v2
du1244 :: T1134 -> T318
du1244 v0
  = let v1 = du1228 (coe v0) in coe (du404 (coe (du1110 (coe v1))))
name1246 = "Algebra.SemiringWithoutOne._.semigroup"
d1246 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T76
d1246 v0 v1 v2 = du1246 v2
du1246 :: T1134 -> T76
du1246 v0
  = let v1 = du1228 (coe v0) in coe (du402 (coe (du1110 (coe v1))))
name1248 = "Algebra.SemiringWithoutOne.+-commutativeMonoid"
d1248 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1134 -> T416
d1248 v0 v1 v2 = du1248 v2
du1248 :: T1134 -> T416
du1248 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1695 v3 v4 v5) erased erased (d1156 (coe v0))
      (d1160 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d842 (coe (d1162 (coe v0))))
name1254 = "Algebra.CommutativeSemiringWithoutOne"
d1254 a0 a1 = ()
data T1254
  = C4977 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny
          MAlonzo.Code.Algebra.Structures.T914
name1272 = "Algebra.CommutativeSemiringWithoutOne.Carrier"
d1272 :: T1254 -> ()
d1272 = erased
name1274 = "Algebra.CommutativeSemiringWithoutOne._≈_"
d1274 :: T1254 -> AgdaAny -> AgdaAny -> ()
d1274 = erased
name1276 = "Algebra.CommutativeSemiringWithoutOne._+_"
d1276 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny
d1276 v0
  = case coe v0 of
      C4977 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1278 = "Algebra.CommutativeSemiringWithoutOne._*_"
d1278 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny
d1278 v0
  = case coe v0 of
      C4977 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1280 = "Algebra.CommutativeSemiringWithoutOne.0#"
d1280 :: T1254 -> AgdaAny
d1280 v0
  = case coe v0 of
      C4977 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1282
  = "Algebra.CommutativeSemiringWithoutOne.isCommutativeSemiringWithoutOne"
d1282 :: T1254 -> MAlonzo.Code.Algebra.Structures.T914
d1282 v0
  = case coe v0 of
      C4977 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1286 = "Algebra.CommutativeSemiringWithoutOne._.assoc"
d1286 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1286 v0 v1 v2 = du1286 v2
du1286 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1286 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe (MAlonzo.Code.Algebra.Structures.d844 (coe v2))))
name1288 = "Algebra.CommutativeSemiringWithoutOne._.*-comm"
d1288 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny
d1288 v0
  = coe (MAlonzo.Code.Algebra.Structures.d928 (coe (d1282 (coe v0))))
name1290 = "Algebra.CommutativeSemiringWithoutOne._.∙-cong"
d1290 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1290 v0 v1 v2 = du1290 v2
du1290 ::
  T1254 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1290 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe (MAlonzo.Code.Algebra.Structures.d844 (coe v2))))))
name1292 = "Algebra.CommutativeSemiringWithoutOne._.∙-congʳ"
d1292 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1292 v0 v1 v2 = du1292 v2
du1292 ::
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1292 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d770 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name1294 = "Algebra.CommutativeSemiringWithoutOne._.∙-congˡ"
d1294 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1294 v0 v1 v2 = du1294 v2
du1294 ::
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1294 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d770 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name1296 = "Algebra.CommutativeSemiringWithoutOne._.isMagma"
d1296 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Algebra.Structures.T84
d1296 v0 v1 v2 = du1296 v2
du1296 :: T1254 -> MAlonzo.Code.Algebra.Structures.T84
du1296 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe (MAlonzo.Code.Algebra.Structures.d844 (coe v2))))
name1298 = "Algebra.CommutativeSemiringWithoutOne._.*-isSemigroup"
d1298 :: T1254 -> MAlonzo.Code.Algebra.Structures.T118
d1298 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d844
         (coe
            (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))))
name1300 = "Algebra.CommutativeSemiringWithoutOne._.*-isSemigroup"
d1300 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Algebra.Structures.T118
d1300 v0 v1 v2 = du1300 v2
du1300 :: T1254 -> MAlonzo.Code.Algebra.Structures.T118
du1300 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe (MAlonzo.Code.Algebra.Structures.d844 (coe v2))
name1302 = "Algebra.CommutativeSemiringWithoutOne._.assoc"
d1302 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1302 v0 v1 v2 = du1302 v2
du1302 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1302 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))
name1304 = "Algebra.CommutativeSemiringWithoutOne._.comm"
d1304 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny
d1304 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe
            (MAlonzo.Code.Algebra.Structures.d842
               (coe
                  (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))))))
name1306 = "Algebra.CommutativeSemiringWithoutOne._.∙-cong"
d1306 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1306 v0 v1 v2 = du1306 v2
du1306 ::
  T1254 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1306 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))))
name1308 = "Algebra.CommutativeSemiringWithoutOne._.∙-congʳ"
d1308 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1308 v0 v1 v2 = du1308 v2
du1308 ::
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1308 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d768 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v7))))
name1310 = "Algebra.CommutativeSemiringWithoutOne._.∙-congˡ"
d1310 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1310 v0 v1 v2 = du1310 v2
du1310 ::
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1310 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d768 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v7))))
name1312 = "Algebra.CommutativeSemiringWithoutOne._.identity"
d1312 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1312 v0 v1 v2 = du1312 v2
du1312 :: T1254 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1312 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v4))))
name1314 = "Algebra.CommutativeSemiringWithoutOne._.identityʳ"
d1314 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> AgdaAny -> AgdaAny
d1314 v0 v1 v2 = du1314 v2
du1314 :: T1254 -> AgdaAny -> AgdaAny
du1314 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d768 (coe v5) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v6))))
name1316 = "Algebra.CommutativeSemiringWithoutOne._.identityˡ"
d1316 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> AgdaAny -> AgdaAny
d1316 v0 v1 v2 = du1316 v2
du1316 :: T1254 -> AgdaAny -> AgdaAny
du1316 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d768 (coe v5) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v6))))
name1318
  = "Algebra.CommutativeSemiringWithoutOne._.+-isCommutativeMonoid"
d1318 :: T1254 -> MAlonzo.Code.Algebra.Structures.T314
d1318 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d842
         (coe
            (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))))
name1320 = "Algebra.CommutativeSemiringWithoutOne._.isMagma"
d1320 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Algebra.Structures.T84
d1320 v0 v1 v2 = du1320 v2
du1320 :: T1254 -> MAlonzo.Code.Algebra.Structures.T84
du1320 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))
name1322 = "Algebra.CommutativeSemiringWithoutOne._.isMonoid"
d1322 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Algebra.Structures.T268
d1322 v0 v1 v2 = du1322 v2
du1322 :: T1254 -> MAlonzo.Code.Algebra.Structures.T268
du1322 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v4))))
name1324 = "Algebra.CommutativeSemiringWithoutOne._.isSemigroup"
d1324 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Algebra.Structures.T118
d1324 v0 v1 v2 = du1324 v2
du1324 :: T1254 -> MAlonzo.Code.Algebra.Structures.T118
du1324 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))
name1326 = "Algebra.CommutativeSemiringWithoutOne._.distrib"
d1326 :: T1254 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1326 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d846
         (coe
            (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))))
name1328 = "Algebra.CommutativeSemiringWithoutOne._.distribʳ"
d1328 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1328 v0 v1 v2 = du1328 v2
du1328 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1328 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d846 (coe v2))))
name1330 = "Algebra.CommutativeSemiringWithoutOne._.isEquivalence"
d1330 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Relation.Binary.Core.T402
d1330 v0 v1 v2 = du1330 v2
du1330 :: T1254 -> MAlonzo.Code.Relation.Binary.Core.T402
du1330 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))))
name1332 = "Algebra.CommutativeSemiringWithoutOne._.isNearSemiring"
d1332 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Algebra.Structures.T752
d1332 v0 v1 v2 = du1332 v2
du1332 :: T1254 -> MAlonzo.Code.Algebra.Structures.T752
du1332 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du860
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d926 (coe v3))))
name1334
  = "Algebra.CommutativeSemiringWithoutOne._.isSemiringWithoutOne"
d1334 :: T1254 -> MAlonzo.Code.Algebra.Structures.T826
d1334 v0
  = coe (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))
name1336 = "Algebra.CommutativeSemiringWithoutOne._.refl"
d1336 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> AgdaAny -> AgdaAny
d1336 v0 v1 v2 = du1336 v2
du1336 :: T1254 -> AgdaAny -> AgdaAny
du1336 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))))))
name1338 = "Algebra.CommutativeSemiringWithoutOne._.reflexive"
d1338 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1338 v0 v1 v2 = du1338 v2
du1338 ::
  T1254 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1338 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d768 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d126 (coe v7) in
    coe
      (\ v9 v10 v11 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v8))) v9)
name1340 = "Algebra.CommutativeSemiringWithoutOne._.setoid"
d1340 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> MAlonzo.Code.Relation.Binary.T128
d1340 v0 v1 v2 = du1340 v2
du1340 :: T1254 -> MAlonzo.Code.Relation.Binary.T128
du1340 v0
  = let v1 = d1276 (coe v0) in
    let v2 = d1280 (coe v0) in
    let v3 = d1282 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d926 (coe v3) in
    let v5
          = MAlonzo.Code.Algebra.Structures.du860
              (coe v1) (coe v2) (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d768 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v7))))
name1342 = "Algebra.CommutativeSemiringWithoutOne._.sym"
d1342 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1342 v0 v1 v2 = du1342 v2
du1342 :: T1254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1342 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))))))
name1344 = "Algebra.CommutativeSemiringWithoutOne._.trans"
d1344 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1254 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1344 v0 v1 v2 = du1344 v2
du1344 ::
  T1254 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1344 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d842 (coe v2))))))))))
name1346 = "Algebra.CommutativeSemiringWithoutOne._.zero"
d1346 :: T1254 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1346 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d848
         (coe
            (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))))
name1348 = "Algebra.CommutativeSemiringWithoutOne._.zeroʳ"
d1348 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> AgdaAny -> AgdaAny
d1348 v0 v1 v2 = du1348 v2
du1348 :: T1254 -> AgdaAny -> AgdaAny
du1348 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d848 (coe v2))))
name1350 = "Algebra.CommutativeSemiringWithoutOne._.zeroˡ"
d1350 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> AgdaAny -> AgdaAny
d1350 v0 v1 v2 = du1350 v2
du1350 :: T1254 -> AgdaAny -> AgdaAny
du1350 v0
  = let v1 = d1282 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d926 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d848 (coe v2))))
name1352
  = "Algebra.CommutativeSemiringWithoutOne.semiringWithoutOne"
d1352 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T1134
d1352 v0 v1 v2 = du1352 v2
du1352 :: T1254 -> T1134
du1352 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C4519 v3 v4 v5 v6) erased erased
      (d1276 (coe v0)) (d1278 (coe v0)) (d1280 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d926 (coe (d1282 (coe v0))))
name1356 = "Algebra.CommutativeSemiringWithoutOne._.magma"
d1356 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T28
d1356 v0 v1 v2 = du1356 v2
du1356 :: T1254 -> T28
du1356 v0
  = let v1 = du1352 (coe v0) in
    let v2 = du1228 (coe v1) in coe (du122 (coe (du1122 (coe v2))))
name1358 = "Algebra.CommutativeSemiringWithoutOne._.rawMagma"
d1358 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T6
d1358 v0 v1 v2 = du1358 v2
du1358 :: T1254 -> T6
du1358 v0
  = let v1 = du1352 (coe v0) in
    let v2 = du1228 (coe v1) in
    let v3 = du1122 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name1360 = "Algebra.CommutativeSemiringWithoutOne._.*-semigroup"
d1360 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T76
d1360 v0 v1 v2 = du1360 v2
du1360 :: T1254 -> T76
du1360 v0
  = let v1 = du1352 (coe v0) in coe (du1122 (coe (du1228 (coe v1))))
name1362
  = "Algebra.CommutativeSemiringWithoutOne._.+-commutativeMonoid"
d1362 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T416
d1362 v0 v1 v2 = du1362 v2
du1362 :: T1254 -> T416
du1362 v0 = coe (du1248 (coe (du1352 (coe v0))))
name1364 = "Algebra.CommutativeSemiringWithoutOne._.magma"
d1364 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T28
d1364 v0 v1 v2 = du1364 v2
du1364 :: T1254 -> T28
du1364 v0
  = let v1 = du1352 (coe v0) in
    let v2 = du1228 (coe v1) in
    let v3 = du1110 (coe v2) in coe (du122 (coe (du402 (coe v3))))
name1366 = "Algebra.CommutativeSemiringWithoutOne._.+-monoid"
d1366 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T344
d1366 v0 v1 v2 = du1366 v2
du1366 :: T1254 -> T344
du1366 v0
  = let v1 = du1352 (coe v0) in coe (du1110 (coe (du1228 (coe v1))))
name1368 = "Algebra.CommutativeSemiringWithoutOne._.rawMagma"
d1368 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T6
d1368 v0 v1 v2 = du1368 v2
du1368 :: T1254 -> T6
du1368 v0
  = let v1 = du1352 (coe v0) in
    let v2 = du1228 (coe v1) in
    let v3 = du1110 (coe v2) in
    let v4 = du402 (coe v3) in coe (du70 (coe (du122 (coe v4))))
name1370 = "Algebra.CommutativeSemiringWithoutOne._.rawMonoid"
d1370 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T318
d1370 v0 v1 v2 = du1370 v2
du1370 :: T1254 -> T318
du1370 v0
  = let v1 = du1352 (coe v0) in
    let v2 = du1228 (coe v1) in coe (du404 (coe (du1110 (coe v2))))
name1372 = "Algebra.CommutativeSemiringWithoutOne._.semigroup"
d1372 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T76
d1372 v0 v1 v2 = du1372 v2
du1372 :: T1254 -> T76
du1372 v0
  = let v1 = du1352 (coe v0) in
    let v2 = du1228 (coe v1) in coe (du402 (coe (du1110 (coe v2))))
name1374 = "Algebra.CommutativeSemiringWithoutOne._.nearSemiring"
d1374 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1254 -> T1030
d1374 v0 v1 v2 = du1374 v2
du1374 :: T1254 -> T1030
du1374 v0 = coe (du1228 (coe (du1352 (coe v0))))
name1380 = "Algebra.RawSemiring"
d1380 a0 a1 = ()
data T1380
  = C5413 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
name1398 = "Algebra.RawSemiring.Carrier"
d1398 :: T1380 -> ()
d1398 = erased
name1400 = "Algebra.RawSemiring._≈_"
d1400 :: T1380 -> AgdaAny -> AgdaAny -> ()
d1400 = erased
name1402 = "Algebra.RawSemiring._+_"
d1402 :: T1380 -> AgdaAny -> AgdaAny -> AgdaAny
d1402 v0
  = case coe v0 of
      C5413 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1404 = "Algebra.RawSemiring._*_"
d1404 :: T1380 -> AgdaAny -> AgdaAny -> AgdaAny
d1404 v0
  = case coe v0 of
      C5413 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1406 = "Algebra.RawSemiring.0#"
d1406 :: T1380 -> AgdaAny
d1406 v0
  = case coe v0 of
      C5413 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1408 = "Algebra.RawSemiring.1#"
d1408 :: T1380 -> AgdaAny
d1408 v0
  = case coe v0 of
      C5413 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1414 = "Algebra.SemiringWithoutAnnihilatingZero"
d1414 a0 a1 = ()
data T1414
  = C5535 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
          MAlonzo.Code.Algebra.Structures.T1002
name1434 = "Algebra.SemiringWithoutAnnihilatingZero.Carrier"
d1434 :: T1414 -> ()
d1434 = erased
name1436 = "Algebra.SemiringWithoutAnnihilatingZero._≈_"
d1436 :: T1414 -> AgdaAny -> AgdaAny -> ()
d1436 = erased
name1438 = "Algebra.SemiringWithoutAnnihilatingZero._+_"
d1438 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny
d1438 v0
  = case coe v0 of
      C5535 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1440 = "Algebra.SemiringWithoutAnnihilatingZero._*_"
d1440 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny
d1440 v0
  = case coe v0 of
      C5535 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1442 = "Algebra.SemiringWithoutAnnihilatingZero.0#"
d1442 :: T1414 -> AgdaAny
d1442 v0
  = case coe v0 of
      C5535 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1444 = "Algebra.SemiringWithoutAnnihilatingZero.1#"
d1444 :: T1414 -> AgdaAny
d1444 v0
  = case coe v0 of
      C5535 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1446
  = "Algebra.SemiringWithoutAnnihilatingZero.isSemiringWithoutAnnihilatingZero"
d1446 :: T1414 -> MAlonzo.Code.Algebra.Structures.T1002
d1446 v0
  = case coe v0 of
      C5535 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
name1450 = "Algebra.SemiringWithoutAnnihilatingZero._.assoc"
d1450 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1450 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1020 (coe (d1446 (coe v0))))))))
name1452 = "Algebra.SemiringWithoutAnnihilatingZero._.∙-cong"
d1452 ::
  T1414 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1452 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1020
                           (coe (d1446 (coe v0))))))))))
name1454 = "Algebra.SemiringWithoutAnnihilatingZero._.∙-congʳ"
d1454 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1454 v0 v1 v2 = du1454 v2
du1454 ::
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1454 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1020 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1456 = "Algebra.SemiringWithoutAnnihilatingZero._.∙-congˡ"
d1456 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1456 v0 v1 v2 = du1456 v2
du1456 ::
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1456 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1020 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1458 = "Algebra.SemiringWithoutAnnihilatingZero._.identity"
d1458 :: T1414 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1458 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d1020 (coe (d1446 (coe v0))))))
name1460 = "Algebra.SemiringWithoutAnnihilatingZero._.identityʳ"
d1460 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> AgdaAny -> AgdaAny
d1460 v0 v1 v2 = du1460 v2
du1460 :: T1414 -> AgdaAny -> AgdaAny
du1460 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1020 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name1462 = "Algebra.SemiringWithoutAnnihilatingZero._.identityˡ"
d1462 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> AgdaAny -> AgdaAny
d1462 v0 v1 v2 = du1462 v2
du1462 :: T1414 -> AgdaAny -> AgdaAny
du1462 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1020 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name1464 = "Algebra.SemiringWithoutAnnihilatingZero._.isMagma"
d1464 :: T1414 -> MAlonzo.Code.Algebra.Structures.T84
d1464 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1020 (coe (d1446 (coe v0))))))))
name1466 = "Algebra.SemiringWithoutAnnihilatingZero._.*-isMonoid"
d1466 :: T1414 -> MAlonzo.Code.Algebra.Structures.T268
d1466 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1020 (coe (d1446 (coe v0))))
name1468 = "Algebra.SemiringWithoutAnnihilatingZero._.isSemigroup"
d1468 :: T1414 -> MAlonzo.Code.Algebra.Structures.T118
d1468 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d1020 (coe (d1446 (coe v0))))))
name1470 = "Algebra.SemiringWithoutAnnihilatingZero._.assoc"
d1470 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1470 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))))))
name1472 = "Algebra.SemiringWithoutAnnihilatingZero._.comm"
d1472 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny
d1472 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe
            (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))))
name1474 = "Algebra.SemiringWithoutAnnihilatingZero._.∙-cong"
d1474 ::
  T1414 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1474 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1018
                           (coe (d1446 (coe v0))))))))))
name1476 = "Algebra.SemiringWithoutAnnihilatingZero._.∙-congʳ"
d1476 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1476 v0 v1 v2 = du1476 v2
du1476 ::
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1476 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1018 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1478 = "Algebra.SemiringWithoutAnnihilatingZero._.∙-congˡ"
d1478 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1478 v0 v1 v2 = du1478 v2
du1478 ::
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1478 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1018 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1480 = "Algebra.SemiringWithoutAnnihilatingZero._.identity"
d1480 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1480 v0 v1 v2 = du1480 v2
du1480 :: T1414 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1480 v0
  = let v1 = d1438 (coe v0) in
    let v2 = d1442 (coe v0) in
    let v3 = d1446 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v3))))
name1482 = "Algebra.SemiringWithoutAnnihilatingZero._.identityʳ"
d1482 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> AgdaAny -> AgdaAny
d1482 v0 v1 v2 = du1482 v2
du1482 :: T1414 -> AgdaAny -> AgdaAny
du1482 v0
  = let v1 = d1438 (coe v0) in
    let v2 = d1442 (coe v0) in
    let v3 = d1446 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du356
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v3))))
name1484 = "Algebra.SemiringWithoutAnnihilatingZero._.identityˡ"
d1484 :: T1414 -> AgdaAny -> AgdaAny
d1484 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d328
         (coe
            (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))))
name1486
  = "Algebra.SemiringWithoutAnnihilatingZero._.+-isCommutativeMonoid"
d1486 :: T1414 -> MAlonzo.Code.Algebra.Structures.T314
d1486 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))
name1488 = "Algebra.SemiringWithoutAnnihilatingZero._.isMagma"
d1488 :: T1414 -> MAlonzo.Code.Algebra.Structures.T84
d1488 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))))))
name1490 = "Algebra.SemiringWithoutAnnihilatingZero._.isMonoid"
d1490 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> MAlonzo.Code.Algebra.Structures.T268
d1490 v0 v1 v2 = du1490 v2
du1490 :: T1414 -> MAlonzo.Code.Algebra.Structures.T268
du1490 v0
  = let v1 = d1438 (coe v0) in
    let v2 = d1442 (coe v0) in
    let v3 = d1446 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v3))))
name1492 = "Algebra.SemiringWithoutAnnihilatingZero._.isSemigroup"
d1492 :: T1414 -> MAlonzo.Code.Algebra.Structures.T118
d1492 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe
            (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))))
name1494 = "Algebra.SemiringWithoutAnnihilatingZero._.distrib"
d1494 :: T1414 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1494 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1022 (coe (d1446 (coe v0))))
name1496 = "Algebra.SemiringWithoutAnnihilatingZero._.distribʳ"
d1496 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1496 v0 v1 v2 = du1496 v2
du1496 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1496 v0
  = let v1 = d1446 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v1))))
name1498 = "Algebra.SemiringWithoutAnnihilatingZero._.distribˡ"
d1498 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1498 v0 v1 v2 = du1498 v2
du1498 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1498 v0
  = let v1 = d1446 (coe v0) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v1))))
name1500
  = "Algebra.SemiringWithoutAnnihilatingZero._.isEquivalence"
d1500 :: T1414 -> MAlonzo.Code.Relation.Binary.Core.T402
d1500 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1018
                           (coe (d1446 (coe v0))))))))))
name1502 = "Algebra.SemiringWithoutAnnihilatingZero._.refl"
d1502 :: T1414 -> AgdaAny -> AgdaAny
d1502 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1018
                                 (coe (d1446 (coe v0))))))))))))
name1504 = "Algebra.SemiringWithoutAnnihilatingZero._.reflexive"
d1504 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1504 v0 v1 v2 = du1504 v2
du1504 ::
  T1414 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1504 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1018 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d126 (coe v3) in
    coe
      (\ v5 v6 v7 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v4))) v5)
name1506 = "Algebra.SemiringWithoutAnnihilatingZero._.setoid"
d1506 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1414 -> MAlonzo.Code.Relation.Binary.T128
d1506 v0 v1 v2 = du1506 v2
du1506 :: T1414 -> MAlonzo.Code.Relation.Binary.T128
du1506 v0
  = let v1 = d1446 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1018 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d326 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1508 = "Algebra.SemiringWithoutAnnihilatingZero._.sym"
d1508 :: T1414 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1508 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1018
                                 (coe (d1446 (coe v0))))))))))))
name1510 = "Algebra.SemiringWithoutAnnihilatingZero._.trans"
d1510 ::
  T1414 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1510 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1018
                                 (coe (d1446 (coe v0))))))))))))
name1512
  = "Algebra.SemiringWithoutAnnihilatingZero.+-commutativeMonoid"
d1512 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T416
d1512 v0 v1 v2 = du1512 v2
du1512 :: T1414 -> T416
du1512 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1695 v3 v4 v5) erased erased (d1438 (coe v0))
      (d1442 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1018 (coe (d1446 (coe v0))))
name1516 = "Algebra.SemiringWithoutAnnihilatingZero._.magma"
d1516 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T28
d1516 v0 v1 v2 = du1516 v2
du1516 :: T1414 -> T28
du1516 v0
  = let v1 = du1512 (coe v0) in
    let v2 = du478 (coe v1) in coe (du122 (coe (du402 (coe v2))))
name1518 = "Algebra.SemiringWithoutAnnihilatingZero._.monoid"
d1518 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T344
d1518 v0 v1 v2 = du1518 v2
du1518 :: T1414 -> T344
du1518 v0 = coe (du478 (coe (du1512 (coe v0))))
name1520 = "Algebra.SemiringWithoutAnnihilatingZero._.rawMagma"
d1520 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T6
d1520 v0 v1 v2 = du1520 v2
du1520 :: T1414 -> T6
du1520 v0
  = let v1 = du1512 (coe v0) in
    let v2 = du478 (coe v1) in
    let v3 = du402 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name1522 = "Algebra.SemiringWithoutAnnihilatingZero._.rawMonoid"
d1522 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T318
d1522 v0 v1 v2 = du1522 v2
du1522 :: T1414 -> T318
du1522 v0
  = let v1 = du1512 (coe v0) in coe (du404 (coe (du478 (coe v1))))
name1524 = "Algebra.SemiringWithoutAnnihilatingZero._.semigroup"
d1524 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T76
d1524 v0 v1 v2 = du1524 v2
du1524 :: T1414 -> T76
du1524 v0
  = let v1 = du1512 (coe v0) in coe (du402 (coe (du478 (coe v1))))
name1526 = "Algebra.SemiringWithoutAnnihilatingZero.*-monoid"
d1526 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T344
d1526 v0 v1 v2 = du1526 v2
du1526 :: T1414 -> T344
du1526 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1365 v3 v4 v5) erased erased (d1440 (coe v0))
      (d1444 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1020 (coe (d1446 (coe v0))))
name1530 = "Algebra.SemiringWithoutAnnihilatingZero._.magma"
d1530 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T28
d1530 v0 v1 v2 = du1530 v2
du1530 :: T1414 -> T28
du1530 v0
  = let v1 = du1526 (coe v0) in coe (du122 (coe (du402 (coe v1))))
name1532 = "Algebra.SemiringWithoutAnnihilatingZero._.rawMagma"
d1532 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T6
d1532 v0 v1 v2 = du1532 v2
du1532 :: T1414 -> T6
du1532 v0
  = let v1 = du1526 (coe v0) in
    let v2 = du402 (coe v1) in coe (du70 (coe (du122 (coe v2))))
name1534 = "Algebra.SemiringWithoutAnnihilatingZero._.rawMonoid"
d1534 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T318
d1534 v0 v1 v2 = du1534 v2
du1534 :: T1414 -> T318
du1534 v0 = coe (du404 (coe (du1526 (coe v0))))
name1536 = "Algebra.SemiringWithoutAnnihilatingZero._.semigroup"
d1536 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1414 -> T76
d1536 v0 v1 v2 = du1536 v2
du1536 :: T1414 -> T76
du1536 v0 = coe (du402 (coe (du1526 (coe v0))))
name1542 = "Algebra.Semiring"
d1542 a0 a1 = ()
data T1542
  = C6067 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
          MAlonzo.Code.Algebra.Structures.T1092
name1562 = "Algebra.Semiring.Carrier"
d1562 :: T1542 -> ()
d1562 = erased
name1564 = "Algebra.Semiring._≈_"
d1564 :: T1542 -> AgdaAny -> AgdaAny -> ()
d1564 = erased
name1566 = "Algebra.Semiring._+_"
d1566 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny
d1566 v0
  = case coe v0 of
      C6067 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1568 = "Algebra.Semiring._*_"
d1568 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny
d1568 v0
  = case coe v0 of
      C6067 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1570 = "Algebra.Semiring.0#"
d1570 :: T1542 -> AgdaAny
d1570 v0
  = case coe v0 of
      C6067 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1572 = "Algebra.Semiring.1#"
d1572 :: T1542 -> AgdaAny
d1572 v0
  = case coe v0 of
      C6067 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1574 = "Algebra.Semiring.isSemiring"
d1574 :: T1542 -> MAlonzo.Code.Algebra.Structures.T1092
d1574 v0
  = case coe v0 of
      C6067 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
name1578 = "Algebra.Semiring._.assoc"
d1578 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1578 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1020
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1106
                           (coe (d1574 (coe v0))))))))))
name1580 = "Algebra.Semiring._.∙-cong"
d1580 ::
  T1542 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1580 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1020
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1106
                                 (coe (d1574 (coe v0))))))))))))
name1582 = "Algebra.Semiring._.∙-congʳ"
d1582 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1582 v0 v1 v2 = du1582 v2
du1582 ::
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1582 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1020 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name1584 = "Algebra.Semiring._.∙-congˡ"
d1584 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1584 v0 v1 v2 = du1584 v2
du1584 ::
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1584 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1020 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name1586 = "Algebra.Semiring._.identity"
d1586 :: T1542 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1586 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d1020
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))))
name1588 = "Algebra.Semiring._.identityʳ"
d1588 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> AgdaAny -> AgdaAny
d1588 v0 v1 v2 = du1588 v2
du1588 :: T1542 -> AgdaAny -> AgdaAny
du1588 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1020 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v3))))
name1590 = "Algebra.Semiring._.identityˡ"
d1590 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> AgdaAny -> AgdaAny
d1590 v0 v1 v2 = du1590 v2
du1590 :: T1542 -> AgdaAny -> AgdaAny
du1590 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1020 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v3))))
name1592 = "Algebra.Semiring._.isMagma"
d1592 :: T1542 -> MAlonzo.Code.Algebra.Structures.T84
d1592 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1020
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1106
                           (coe (d1574 (coe v0))))))))))
name1594 = "Algebra.Semiring._.*-isMonoid"
d1594 :: T1542 -> MAlonzo.Code.Algebra.Structures.T268
d1594 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1020
         (coe
            (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))
name1596 = "Algebra.Semiring._.isSemigroup"
d1596 :: T1542 -> MAlonzo.Code.Algebra.Structures.T118
d1596 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d1020
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))))
name1598 = "Algebra.Semiring._.assoc"
d1598 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1598 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1018
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1106
                           (coe (d1574 (coe v0))))))))))
name1600 = "Algebra.Semiring._.comm"
d1600 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny
d1600 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe
            (MAlonzo.Code.Algebra.Structures.d1018
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))))
name1602 = "Algebra.Semiring._.∙-cong"
d1602 ::
  T1542 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1602 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1018
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1106
                                 (coe (d1574 (coe v0))))))))))))
name1604 = "Algebra.Semiring._.∙-congʳ"
d1604 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1604 v0 v1 v2 = du1604 v2
du1604 ::
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1604 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1018 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d326 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name1606 = "Algebra.Semiring._.∙-congˡ"
d1606 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1606 v0 v1 v2 = du1606 v2
du1606 ::
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1606 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1018 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d326 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name1608 = "Algebra.Semiring._.identity"
d1608 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1608 v0 v1 v2 = du1608 v2
du1608 :: T1542 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1608 v0
  = let v1 = d1566 (coe v0) in
    let v2 = d1570 (coe v0) in
    let v3 = d1574 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d1106 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v4))))
name1610 = "Algebra.Semiring._.identityʳ"
d1610 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> AgdaAny -> AgdaAny
d1610 v0 v1 v2 = du1610 v2
du1610 :: T1542 -> AgdaAny -> AgdaAny
du1610 v0
  = let v1 = d1566 (coe v0) in
    let v2 = d1570 (coe v0) in
    let v3 = d1574 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d1106 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du356
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v4))))
name1612 = "Algebra.Semiring._.identityˡ"
d1612 :: T1542 -> AgdaAny -> AgdaAny
d1612 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d328
         (coe
            (MAlonzo.Code.Algebra.Structures.d1018
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))))
name1614 = "Algebra.Semiring._.+-isCommutativeMonoid"
d1614 :: T1542 -> MAlonzo.Code.Algebra.Structures.T314
d1614 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1018
         (coe
            (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))
name1616 = "Algebra.Semiring._.isMagma"
d1616 :: T1542 -> MAlonzo.Code.Algebra.Structures.T84
d1616 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1018
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1106
                           (coe (d1574 (coe v0))))))))))
name1618 = "Algebra.Semiring._.isMonoid"
d1618 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> MAlonzo.Code.Algebra.Structures.T268
d1618 v0 v1 v2 = du1618 v2
du1618 :: T1542 -> MAlonzo.Code.Algebra.Structures.T268
du1618 v0
  = let v1 = d1566 (coe v0) in
    let v2 = d1570 (coe v0) in
    let v3 = d1574 (coe v0) in
    let v4 = MAlonzo.Code.Algebra.Structures.d1106 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v4))))
name1620 = "Algebra.Semiring._.isSemigroup"
d1620 :: T1542 -> MAlonzo.Code.Algebra.Structures.T118
d1620 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe
            (MAlonzo.Code.Algebra.Structures.d1018
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))))
name1622 = "Algebra.Semiring._.distrib"
d1622 :: T1542 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1622 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1022
         (coe
            (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))))
name1624 = "Algebra.Semiring._.distribʳ"
d1624 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1624 v0 v1 v2 = du1624 v2
du1624 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1624 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v2))))
name1626 = "Algebra.Semiring._.distribˡ"
d1626 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1626 v0 v1 v2 = du1626 v2
du1626 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1626 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v2))))
name1628 = "Algebra.Semiring._.isEquivalence"
d1628 :: T1542 -> MAlonzo.Code.Relation.Binary.Core.T402
d1628 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1018
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1106
                                 (coe (d1574 (coe v0))))))))))))
name1630 = "Algebra.Semiring._.isNearSemiring"
d1630 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> MAlonzo.Code.Algebra.Structures.T752
d1630 v0 v1 v2 = du1630 v2
du1630 :: T1542 -> MAlonzo.Code.Algebra.Structures.T752
du1630 v0
  = let v1 = d1566 (coe v0) in
    let v2 = d1570 (coe v0) in
    let v3 = d1574 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du860
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.du1174 (coe v3))))
name1632 = "Algebra.Semiring._.isSemiringWithoutAnnihilatingZero"
d1632 :: T1542 -> MAlonzo.Code.Algebra.Structures.T1002
d1632 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))
name1634 = "Algebra.Semiring._.isSemiringWithoutOne"
d1634 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> MAlonzo.Code.Algebra.Structures.T826
d1634 v0 v1 v2 = du1634 v2
du1634 :: T1542 -> MAlonzo.Code.Algebra.Structures.T826
du1634 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1174 (coe (d1574 (coe v0))))
name1636 = "Algebra.Semiring._.refl"
d1636 :: T1542 -> AgdaAny -> AgdaAny
d1636 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1018
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1106
                                       (coe (d1574 (coe v0))))))))))))))
name1638 = "Algebra.Semiring._.reflexive"
d1638 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1638 v0 v1 v2 = du1638 v2
du1638 ::
  T1542 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1638 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1018 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d326 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d126 (coe v4) in
    coe
      (\ v6 v7 v8 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v5))) v6)
name1640 = "Algebra.Semiring._.setoid"
d1640 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1542 -> MAlonzo.Code.Relation.Binary.T128
d1640 v0 v1 v2 = du1640 v2
du1640 :: T1542 -> MAlonzo.Code.Relation.Binary.T128
du1640 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1106 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1018 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d326 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name1642 = "Algebra.Semiring._.sym"
d1642 :: T1542 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1642 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1018
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1106
                                       (coe (d1574 (coe v0))))))))))))))
name1644 = "Algebra.Semiring._.trans"
d1644 ::
  T1542 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1644 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1018
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1106
                                       (coe (d1574 (coe v0))))))))))))))
name1646 = "Algebra.Semiring._.zero"
d1646 :: T1542 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1646 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1108 (coe (d1574 (coe v0))))
name1648 = "Algebra.Semiring._.zeroʳ"
d1648 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> AgdaAny -> AgdaAny
d1648 v0 v1 v2 = du1648 v2
du1648 :: T1542 -> AgdaAny -> AgdaAny
du1648 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.du1174 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d848 (coe v2))))
name1650 = "Algebra.Semiring._.zeroˡ"
d1650 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> AgdaAny -> AgdaAny
d1650 v0 v1 v2 = du1650 v2
du1650 :: T1542 -> AgdaAny -> AgdaAny
du1650 v0
  = let v1 = d1574 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.du1174 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d848 (coe v2))))
name1652 = "Algebra.Semiring.rawSemiring"
d1652 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T1380
d1652 v0 v1 v2 = du1652 v2
du1652 :: T1542 -> T1380
du1652 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C5413 v3 v4 v5 v6) erased erased
      (d1566 (coe v0)) (d1568 (coe v0)) (d1570 (coe v0)) (d1572 (coe v0))
name1654 = "Algebra.Semiring.semiringWithoutAnnihilatingZero"
d1654 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T1414
d1654 v0 v1 v2 = du1654 v2
du1654 :: T1542 -> T1414
du1654 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 v7 -> C5535 v3 v4 v5 v6 v7) erased erased
      (d1566 (coe v0)) (d1568 (coe v0)) (d1570 (coe v0)) (d1572 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1106 (coe (d1574 (coe v0))))
name1658 = "Algebra.Semiring._.magma"
d1658 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T28
d1658 v0 v1 v2 = du1658 v2
du1658 :: T1542 -> T28
du1658 v0
  = let v1 = du1654 (coe v0) in
    let v2 = du1526 (coe v1) in coe (du122 (coe (du402 (coe v2))))
name1660 = "Algebra.Semiring._.*-monoid"
d1660 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T344
d1660 v0 v1 v2 = du1660 v2
du1660 :: T1542 -> T344
du1660 v0 = coe (du1526 (coe (du1654 (coe v0))))
name1662 = "Algebra.Semiring._.rawMagma"
d1662 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T6
d1662 v0 v1 v2 = du1662 v2
du1662 :: T1542 -> T6
du1662 v0
  = let v1 = du1654 (coe v0) in
    let v2 = du1526 (coe v1) in
    let v3 = du402 (coe v2) in coe (du70 (coe (du122 (coe v3))))
name1664 = "Algebra.Semiring._.rawMonoid"
d1664 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T318
d1664 v0 v1 v2 = du1664 v2
du1664 :: T1542 -> T318
du1664 v0
  = let v1 = du1654 (coe v0) in coe (du404 (coe (du1526 (coe v1))))
name1666 = "Algebra.Semiring._.semigroup"
d1666 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T76
d1666 v0 v1 v2 = du1666 v2
du1666 :: T1542 -> T76
du1666 v0
  = let v1 = du1654 (coe v0) in coe (du402 (coe (du1526 (coe v1))))
name1668 = "Algebra.Semiring._.+-commutativeMonoid"
d1668 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T416
d1668 v0 v1 v2 = du1668 v2
du1668 :: T1542 -> T416
du1668 v0 = coe (du1512 (coe (du1654 (coe v0))))
name1670 = "Algebra.Semiring._.magma"
d1670 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T28
d1670 v0 v1 v2 = du1670 v2
du1670 :: T1542 -> T28
du1670 v0
  = let v1 = du1654 (coe v0) in
    let v2 = du1512 (coe v1) in
    let v3 = du478 (coe v2) in coe (du122 (coe (du402 (coe v3))))
name1672 = "Algebra.Semiring._.monoid"
d1672 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T344
d1672 v0 v1 v2 = du1672 v2
du1672 :: T1542 -> T344
du1672 v0
  = let v1 = du1654 (coe v0) in coe (du478 (coe (du1512 (coe v1))))
name1674 = "Algebra.Semiring._.rawMagma"
d1674 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T6
d1674 v0 v1 v2 = du1674 v2
du1674 :: T1542 -> T6
du1674 v0
  = let v1 = du1654 (coe v0) in
    let v2 = du1512 (coe v1) in
    let v3 = du478 (coe v2) in
    let v4 = du402 (coe v3) in coe (du70 (coe (du122 (coe v4))))
name1676 = "Algebra.Semiring._.rawMonoid"
d1676 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T318
d1676 v0 v1 v2 = du1676 v2
du1676 :: T1542 -> T318
du1676 v0
  = let v1 = du1654 (coe v0) in
    let v2 = du1512 (coe v1) in coe (du404 (coe (du478 (coe v2))))
name1678 = "Algebra.Semiring._.semigroup"
d1678 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T76
d1678 v0 v1 v2 = du1678 v2
du1678 :: T1542 -> T76
du1678 v0
  = let v1 = du1654 (coe v0) in
    let v2 = du1512 (coe v1) in coe (du402 (coe (du478 (coe v2))))
name1680 = "Algebra.Semiring.semiringWithoutOne"
d1680 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T1134
d1680 v0 v1 v2 = du1680 v2
du1680 :: T1542 -> T1134
du1680 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C4519 v3 v4 v5 v6) erased erased
      (d1566 (coe v0)) (d1568 (coe v0)) (d1570 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du1174 (coe (d1574 (coe v0))))
name1684 = "Algebra.Semiring._.nearSemiring"
d1684 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1542 -> T1030
d1684 v0 v1 v2 = du1684 v2
du1684 :: T1542 -> T1030
du1684 v0 = coe (du1228 (coe (du1680 (coe v0))))
name1690 = "Algebra.CommutativeSemiring"
d1690 a0 a1 = ()
data T1690
  = C6707 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) AgdaAny AgdaAny
          MAlonzo.Code.Algebra.Structures.T1192
name1710 = "Algebra.CommutativeSemiring.Carrier"
d1710 :: T1690 -> ()
d1710 = erased
name1712 = "Algebra.CommutativeSemiring._≈_"
d1712 :: T1690 -> AgdaAny -> AgdaAny -> ()
d1712 = erased
name1714 = "Algebra.CommutativeSemiring._+_"
d1714 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny
d1714 v0
  = case coe v0 of
      C6707 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1716 = "Algebra.CommutativeSemiring._*_"
d1716 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny
d1716 v0
  = case coe v0 of
      C6707 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1718 = "Algebra.CommutativeSemiring.0#"
d1718 :: T1690 -> AgdaAny
d1718 v0
  = case coe v0 of
      C6707 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1720 = "Algebra.CommutativeSemiring.1#"
d1720 :: T1690 -> AgdaAny
d1720 v0
  = case coe v0 of
      C6707 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1722 = "Algebra.CommutativeSemiring.isCommutativeSemiring"
d1722 :: T1690 -> MAlonzo.Code.Algebra.Structures.T1192
d1722 v0
  = case coe v0 of
      C6707 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
name1726 = "Algebra.CommutativeSemiring._.assoc"
d1726 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1726 v0 v1 v2 = du1726 v2
du1726 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1726 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d1212 (coe v1))))))
name1728 = "Algebra.CommutativeSemiring._.comm"
d1728 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny
d1728 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe
            (MAlonzo.Code.Algebra.Structures.d1212 (coe (d1722 (coe v0))))))
name1730 = "Algebra.CommutativeSemiring._.∙-cong"
d1730 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1730 v0 v1 v2 = du1730 v2
du1730 ::
  T1690 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1730 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d1212 (coe v1))))))))
name1732 = "Algebra.CommutativeSemiring._.∙-congʳ"
d1732 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1732 v0 v1 v2 = du1732 v2
du1732 ::
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1732 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1020 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d278 (coe v8) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v9))))
name1734 = "Algebra.CommutativeSemiring._.∙-congˡ"
d1734 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1734 v0 v1 v2 = du1734 v2
du1734 ::
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1734 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1020 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d278 (coe v8) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v9))))
name1736 = "Algebra.CommutativeSemiring._.identity"
d1736 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1736 v0 v1 v2 = du1736 v2
du1736 :: T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1736 v0
  = let v1 = d1716 (coe v0) in
    let v2 = d1720 (coe v0) in
    let v3 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1212 (coe v3))))
name1738 = "Algebra.CommutativeSemiring._.identityʳ"
d1738 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> AgdaAny -> AgdaAny
d1738 v0 v1 v2 = du1738 v2
du1738 :: T1690 -> AgdaAny -> AgdaAny
du1738 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1020 (coe v7) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v8))))
name1740 = "Algebra.CommutativeSemiring._.identityˡ"
d1740 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> AgdaAny -> AgdaAny
d1740 v0 v1 v2 = du1740 v2
du1740 :: T1690 -> AgdaAny -> AgdaAny
du1740 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1020 (coe v7) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v8))))
name1742 = "Algebra.CommutativeSemiring._.*-isCommutativeMonoid"
d1742 :: T1690 -> MAlonzo.Code.Algebra.Structures.T314
d1742 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1212 (coe (d1722 (coe v0))))
name1744 = "Algebra.CommutativeSemiring._.isMagma"
d1744 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T84
d1744 v0 v1 v2 = du1744 v2
du1744 :: T1690 -> MAlonzo.Code.Algebra.Structures.T84
du1744 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d1212 (coe v1))))))
name1746 = "Algebra.CommutativeSemiring._.*-isMonoid"
d1746 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T268
d1746 v0 v1 v2 = du1746 v2
du1746 :: T1690 -> MAlonzo.Code.Algebra.Structures.T268
du1746 v0
  = let v1 = d1716 (coe v0) in
    let v2 = d1720 (coe v0) in
    let v3 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d1212 (coe v3))))
name1748 = "Algebra.CommutativeSemiring._.isSemigroup"
d1748 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T118
d1748 v0 v1 v2 = du1748 v2
du1748 :: T1690 -> MAlonzo.Code.Algebra.Structures.T118
du1748 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe (MAlonzo.Code.Algebra.Structures.d1212 (coe v1))))
name1750 = "Algebra.CommutativeSemiring._.assoc"
d1750 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1750 v0 v1 v2 = du1750 v2
du1750 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1750 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))
name1752 = "Algebra.CommutativeSemiring._.comm"
d1752 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny
d1752 v0 v1 v2 = du1752 v2
du1752 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny
du1752 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d330
         (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))
name1754 = "Algebra.CommutativeSemiring._.∙-cong"
d1754 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1754 v0 v1 v2 = du1754 v2
du1754 ::
  T1690 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1754 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))))
name1756 = "Algebra.CommutativeSemiring._.∙-congʳ"
d1756 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1756 v0 v1 v2 = du1756 v2
du1756 ::
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1756 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1018 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d326 (coe v8) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v9))))
name1758 = "Algebra.CommutativeSemiring._.∙-congˡ"
d1758 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1758 v0 v1 v2 = du1758 v2
du1758 ::
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1758 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1018 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d326 (coe v8) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v9))))
name1760 = "Algebra.CommutativeSemiring._.identity"
d1760 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1760 v0 v1 v2 = du1760 v2
du1760 :: T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1760 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du358
         (coe v1) (coe v3)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v7))))
name1762 = "Algebra.CommutativeSemiring._.identityʳ"
d1762 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> AgdaAny -> AgdaAny
d1762 v0 v1 v2 = du1762 v2
du1762 :: T1690 -> AgdaAny -> AgdaAny
du1762 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du356
         (coe v1) (coe v3)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v7))))
name1764 = "Algebra.CommutativeSemiring._.identityˡ"
d1764 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> AgdaAny -> AgdaAny
d1764 v0 v1 v2 = du1764 v2
du1764 :: T1690 -> AgdaAny -> AgdaAny
du1764 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d328
         (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))
name1766 = "Algebra.CommutativeSemiring._.+-isCommutativeMonoid"
d1766 :: T1690 -> MAlonzo.Code.Algebra.Structures.T314
d1766 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1210 (coe (d1722 (coe v0))))
name1768 = "Algebra.CommutativeSemiring._.isMagma"
d1768 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T84
d1768 v0 v1 v2 = du1768 v2
du1768 :: T1690 -> MAlonzo.Code.Algebra.Structures.T84
du1768 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d326
               (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))
name1770 = "Algebra.CommutativeSemiring._.isMonoid"
d1770 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T268
d1770 v0 v1 v2 = du1770 v2
du1770 :: T1690 -> MAlonzo.Code.Algebra.Structures.T268
du1770 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du360
         (coe v1) (coe v3)
         (coe (MAlonzo.Code.Algebra.Structures.d1018 (coe v7))))
name1772 = "Algebra.CommutativeSemiring._.isSemigroup"
d1772 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T118
d1772 v0 v1 v2 = du1772 v2
du1772 :: T1690 -> MAlonzo.Code.Algebra.Structures.T118
du1772 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d326
         (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))
name1774 = "Algebra.CommutativeSemiring._.distrib"
d1774 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1774 v0 v1 v2 = du1774 v2
du1774 :: T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1774 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1292
         (coe (d1714 (coe v0))) (coe (d1716 (coe v0)))
         (coe (d1722 (coe v0))))
name1776 = "Algebra.CommutativeSemiring._.distribʳ"
d1776 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1776 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1214 (coe (d1722 (coe v0))))
name1778 = "Algebra.CommutativeSemiring._.distribˡ"
d1778 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1778 v0 v1 v2 = du1778 v2
du1778 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1778 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1290
         (coe (d1714 (coe v0))) (coe (d1716 (coe v0)))
         (coe (d1722 (coe v0))))
name1780
  = "Algebra.CommutativeSemiring._.isCommutativeSemiringWithoutOne"
d1780 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T914
d1780 v0 v1 v2 = du1780 v2
du1780 :: T1690 -> MAlonzo.Code.Algebra.Structures.T914
du1780 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1362
         (coe (d1714 (coe v0))) (coe (d1716 (coe v0)))
         (coe (d1718 (coe v0))) (coe (d1720 (coe v0)))
         (coe (d1722 (coe v0))))
name1782 = "Algebra.CommutativeSemiring._.isEquivalence"
d1782 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Relation.Binary.Core.T402
d1782 v0 v1 v2 = du1782 v2
du1782 :: T1690 -> MAlonzo.Code.Relation.Binary.Core.T402
du1782 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d326
                     (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))))
name1784 = "Algebra.CommutativeSemiring._.isNearSemiring"
d1784 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T752
d1784 v0 v1 v2 = du1784 v2
du1784 :: T1690 -> MAlonzo.Code.Algebra.Structures.T752
du1784 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du860
         (coe v1) (coe v3)
         (coe (MAlonzo.Code.Algebra.Structures.du1174 (coe v6))))
name1786 = "Algebra.CommutativeSemiring._.isSemiring"
d1786 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T1092
d1786 v0 v1 v2 = du1786 v2
du1786 :: T1690 -> MAlonzo.Code.Algebra.Structures.T1092
du1786 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1298
         (coe (d1714 (coe v0))) (coe (d1716 (coe v0)))
         (coe (d1718 (coe v0))) (coe (d1720 (coe v0)))
         (coe (d1722 (coe v0))))
name1788
  = "Algebra.CommutativeSemiring._.isSemiringWithoutAnnihilatingZero"
d1788 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T1002
d1788 v0 v1 v2 = du1788 v2
du1788 :: T1690 -> MAlonzo.Code.Algebra.Structures.T1002
du1788 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.d1106
         (coe
            (MAlonzo.Code.Algebra.Structures.du1298
               (coe v1) (coe v2) (coe v3) (coe v4) (coe v5))))
name1790 = "Algebra.CommutativeSemiring._.isSemiringWithoutOne"
d1790 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Algebra.Structures.T826
d1790 v0 v1 v2 = du1790 v2
du1790 :: T1690 -> MAlonzo.Code.Algebra.Structures.T826
du1790 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1174
         (coe
            (MAlonzo.Code.Algebra.Structures.du1298
               (coe v1) (coe v2) (coe v3) (coe v4) (coe v5))))
name1792 = "Algebra.CommutativeSemiring._.refl"
d1792 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> AgdaAny -> AgdaAny
d1792 v0 v1 v2 = du1792 v2
du1792 :: T1690 -> AgdaAny -> AgdaAny
du1792 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))))))
name1794 = "Algebra.CommutativeSemiring._.reflexive"
d1794 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1794 v0 v1 v2 = du1794 v2
du1794 ::
  T1690 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1794 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1018 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d326 (coe v8) in
    let v10 = MAlonzo.Code.Algebra.Structures.d126 (coe v9) in
    coe
      (\ v11 v12 v13 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v10))) v11)
name1796 = "Algebra.CommutativeSemiring._.setoid"
d1796 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Relation.Binary.T128
d1796 v0 v1 v2 = du1796 v2
du1796 :: T1690 -> MAlonzo.Code.Relation.Binary.T128
du1796 v0
  = let v1 = d1714 (coe v0) in
    let v2 = d1716 (coe v0) in
    let v3 = d1718 (coe v0) in
    let v4 = d1720 (coe v0) in
    let v5 = d1722 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1298
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1018 (coe v7) in
    let v9 = MAlonzo.Code.Algebra.Structures.d326 (coe v8) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v9))))
name1798 = "Algebra.CommutativeSemiring._.sym"
d1798 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1798 v0 v1 v2 = du1798 v2
du1798 :: T1690 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1798 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))))))
name1800 = "Algebra.CommutativeSemiring._.trans"
d1800 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1800 v0 v1 v2 = du1800 v2
du1800 ::
  T1690 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1800 v0
  = let v1 = d1722 (coe v0) in
    coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d326
                           (coe (MAlonzo.Code.Algebra.Structures.d1210 (coe v1))))))))))
name1802 = "Algebra.CommutativeSemiring._.zero"
d1802 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1802 v0 v1 v2 = du1802 v2
du1802 :: T1690 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du1802 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1296
         (coe (d1716 (coe v0))) (coe (d1718 (coe v0)))
         (coe (d1722 (coe v0))))
name1804 = "Algebra.CommutativeSemiring._.zeroʳ"
d1804 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> AgdaAny -> AgdaAny
d1804 v0 v1 v2 = du1804 v2
du1804 :: T1690 -> AgdaAny -> AgdaAny
du1804 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1294
         (coe (d1716 (coe v0))) (coe (d1718 (coe v0)))
         (coe (d1722 (coe v0))))
name1806 = "Algebra.CommutativeSemiring._.zeroˡ"
d1806 :: T1690 -> AgdaAny -> AgdaAny
d1806 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1216 (coe (d1722 (coe v0))))
name1808 = "Algebra.CommutativeSemiring.semiring"
d1808 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T1542
d1808 v0 v1 v2 = du1808 v2
du1808 :: T1690 -> T1542
du1808 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 v7 -> C6067 v3 v4 v5 v6 v7) erased erased
      (d1714 (coe v0)) (d1716 (coe v0)) (d1718 (coe v0)) (d1720 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du1298
         (coe (d1714 (coe v0))) (coe (d1716 (coe v0)))
         (coe (d1718 (coe v0))) (coe (d1720 (coe v0)))
         (coe (d1722 (coe v0))))
name1812 = "Algebra.CommutativeSemiring._.magma"
d1812 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T28
d1812 v0 v1 v2 = du1812 v2
du1812 :: T1690 -> T28
du1812 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1526 (coe v2) in coe (du122 (coe (du402 (coe v3))))
name1814 = "Algebra.CommutativeSemiring._.*-monoid"
d1814 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T344
d1814 v0 v1 v2 = du1814 v2
du1814 :: T1690 -> T344
du1814 v0
  = let v1 = du1808 (coe v0) in coe (du1526 (coe (du1654 (coe v1))))
name1816 = "Algebra.CommutativeSemiring._.rawMagma"
d1816 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T6
d1816 v0 v1 v2 = du1816 v2
du1816 :: T1690 -> T6
du1816 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1526 (coe v2) in
    let v4 = du402 (coe v3) in coe (du70 (coe (du122 (coe v4))))
name1818 = "Algebra.CommutativeSemiring._.rawMonoid"
d1818 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T318
d1818 v0 v1 v2 = du1818 v2
du1818 :: T1690 -> T318
du1818 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in coe (du404 (coe (du1526 (coe v2))))
name1820 = "Algebra.CommutativeSemiring._.semigroup"
d1820 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T76
d1820 v0 v1 v2 = du1820 v2
du1820 :: T1690 -> T76
du1820 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in coe (du402 (coe (du1526 (coe v2))))
name1822 = "Algebra.CommutativeSemiring._.+-commutativeMonoid"
d1822 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T416
d1822 v0 v1 v2 = du1822 v2
du1822 :: T1690 -> T416
du1822 v0
  = let v1 = du1808 (coe v0) in coe (du1512 (coe (du1654 (coe v1))))
name1824 = "Algebra.CommutativeSemiring._.magma"
d1824 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T28
d1824 v0 v1 v2 = du1824 v2
du1824 :: T1690 -> T28
du1824 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in
    let v4 = du478 (coe v3) in coe (du122 (coe (du402 (coe v4))))
name1826 = "Algebra.CommutativeSemiring._.monoid"
d1826 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T344
d1826 v0 v1 v2 = du1826 v2
du1826 :: T1690 -> T344
du1826 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in coe (du478 (coe (du1512 (coe v2))))
name1828 = "Algebra.CommutativeSemiring._.rawMagma"
d1828 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T6
d1828 v0 v1 v2 = du1828 v2
du1828 :: T1690 -> T6
du1828 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in
    let v4 = du478 (coe v3) in
    let v5 = du402 (coe v4) in coe (du70 (coe (du122 (coe v5))))
name1830 = "Algebra.CommutativeSemiring._.rawMonoid"
d1830 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T318
d1830 v0 v1 v2 = du1830 v2
du1830 :: T1690 -> T318
du1830 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in coe (du404 (coe (du478 (coe v3))))
name1832 = "Algebra.CommutativeSemiring._.semigroup"
d1832 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T76
d1832 v0 v1 v2 = du1832 v2
du1832 :: T1690 -> T76
du1832 v0
  = let v1 = du1808 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in coe (du402 (coe (du478 (coe v3))))
name1834 = "Algebra.CommutativeSemiring._.nearSemiring"
d1834 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T1030
d1834 v0 v1 v2 = du1834 v2
du1834 :: T1690 -> T1030
du1834 v0
  = let v1 = du1808 (coe v0) in coe (du1228 (coe (du1680 (coe v1))))
name1836 = "Algebra.CommutativeSemiring._.rawSemiring"
d1836 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T1380
d1836 v0 v1 v2 = du1836 v2
du1836 :: T1690 -> T1380
du1836 v0 = coe (du1652 (coe (du1808 (coe v0))))
name1838
  = "Algebra.CommutativeSemiring._.semiringWithoutAnnihilatingZero"
d1838 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T1414
d1838 v0 v1 v2 = du1838 v2
du1838 :: T1690 -> T1414
du1838 v0 = coe (du1654 (coe (du1808 (coe v0))))
name1840 = "Algebra.CommutativeSemiring._.semiringWithoutOne"
d1840 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T1134
d1840 v0 v1 v2 = du1840 v2
du1840 :: T1690 -> T1134
du1840 v0 = coe (du1680 (coe (du1808 (coe v0))))
name1842 = "Algebra.CommutativeSemiring.*-commutativeMonoid"
d1842 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T416
d1842 v0 v1 v2 = du1842 v2
du1842 :: T1690 -> T416
du1842 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C1695 v3 v4 v5) erased erased (d1716 (coe v0))
      (d1720 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1212 (coe (d1722 (coe v0))))
name1844
  = "Algebra.CommutativeSemiring.commutativeSemiringWithoutOne"
d1844 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1690 -> T1254
d1844 v0 v1 v2 = du1844 v2
du1844 :: T1690 -> T1254
du1844 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C4977 v3 v4 v5 v6) erased erased
      (d1714 (coe v0)) (d1716 (coe v0)) (d1718 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du1362
         (coe (d1714 (coe v0))) (coe (d1716 (coe v0)))
         (coe (d1718 (coe v0))) (coe (d1720 (coe v0)))
         (coe (d1722 (coe v0))))
name1848 = "Algebra.RawRing"
d1848 a0 = ()
data T1848
  = C7311 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
          AgdaAny
name1864 = "Algebra.RawRing.Carrier"
d1864 :: T1848 -> ()
d1864 = erased
name1866 = "Algebra.RawRing._+_"
d1866 :: T1848 -> AgdaAny -> AgdaAny -> AgdaAny
d1866 v0
  = case coe v0 of
      C7311 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name1868 = "Algebra.RawRing._*_"
d1868 :: T1848 -> AgdaAny -> AgdaAny -> AgdaAny
d1868 v0
  = case coe v0 of
      C7311 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1870 = "Algebra.RawRing.-_"
d1870 :: T1848 -> AgdaAny -> AgdaAny
d1870 v0
  = case coe v0 of
      C7311 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1872 = "Algebra.RawRing.0#"
d1872 :: T1848 -> AgdaAny
d1872 v0
  = case coe v0 of
      C7311 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1874 = "Algebra.RawRing.1#"
d1874 :: T1848 -> AgdaAny
d1874 v0
  = case coe v0 of
      C7311 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1880 = "Algebra.Ring"
d1880 a0 a1 = ()
data T1880
  = C7407 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
          AgdaAny MAlonzo.Code.Algebra.Structures.T1374
name1902 = "Algebra.Ring.Carrier"
d1902 :: T1880 -> ()
d1902 = erased
name1904 = "Algebra.Ring._≈_"
d1904 :: T1880 -> AgdaAny -> AgdaAny -> ()
d1904 = erased
name1906 = "Algebra.Ring._+_"
d1906 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny
d1906 v0
  = case coe v0 of
      C7407 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name1908 = "Algebra.Ring._*_"
d1908 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny
d1908 v0
  = case coe v0 of
      C7407 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name1910 = "Algebra.Ring.-_"
d1910 :: T1880 -> AgdaAny -> AgdaAny
d1910 v0
  = case coe v0 of
      C7407 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name1912 = "Algebra.Ring.0#"
d1912 :: T1880 -> AgdaAny
d1912 v0
  = case coe v0 of
      C7407 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name1914 = "Algebra.Ring.1#"
d1914 :: T1880 -> AgdaAny
d1914 v0
  = case coe v0 of
      C7407 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
name1916 = "Algebra.Ring.isRing"
d1916 :: T1880 -> MAlonzo.Code.Algebra.Structures.T1374
d1916 v0
  = case coe v0 of
      C7407 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
name1920 = "Algebra.Ring._._-_"
d1920 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny
d1920 v0 v1 v2 = du1920 v2
du1920 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny
du1920 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1910 (coe v0) in coe (\ v3 v4 -> coe v1 v3 (coe v2 v4))
name1922 = "Algebra.Ring._.assoc"
d1922 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1922 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1394 (coe (d1916 (coe v0))))))))
name1924 = "Algebra.Ring._.∙-cong"
d1924 ::
  T1880 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1924 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1394
                           (coe (d1916 (coe v0))))))))))
name1926 = "Algebra.Ring._.∙-congʳ"
d1926 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1926 v0 v1 v2 = du1926 v2
du1926 ::
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1926 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1394 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1928 = "Algebra.Ring._.∙-congˡ"
d1928 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1928 v0 v1 v2 = du1928 v2
du1928 ::
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1928 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1394 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d278 (coe v2) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v3))))
name1930 = "Algebra.Ring._.identity"
d1930 :: T1880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1930 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d1394 (coe (d1916 (coe v0))))))
name1932 = "Algebra.Ring._.identityʳ"
d1932 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d1932 v0 v1 v2 = du1932 v2
du1932 :: T1880 -> AgdaAny -> AgdaAny
du1932 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1394 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name1934 = "Algebra.Ring._.identityˡ"
d1934 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d1934 v0 v1 v2 = du1934 v2
du1934 :: T1880 -> AgdaAny -> AgdaAny
du1934 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1394 (coe v1) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v2))))
name1936 = "Algebra.Ring._.isMagma"
d1936 :: T1880 -> MAlonzo.Code.Algebra.Structures.T84
d1936 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1394 (coe (d1916 (coe v0))))))))
name1938 = "Algebra.Ring._.*-isMonoid"
d1938 :: T1880 -> MAlonzo.Code.Algebra.Structures.T268
d1938 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1394 (coe (d1916 (coe v0))))
name1940 = "Algebra.Ring._.isSemigroup"
d1940 :: T1880 -> MAlonzo.Code.Algebra.Structures.T118
d1940 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d1394 (coe (d1916 (coe v0))))))
name1942 = "Algebra.Ring._.assoc"
d1942 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1942 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d562
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1392
                                 (coe (d1916 (coe v0))))))))))))
name1944 = "Algebra.Ring._.comm"
d1944 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny
d1944 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d564
         (coe
            (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))))
name1946 = "Algebra.Ring._.∙-cong"
d1946 ::
  T1880 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1946 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d562
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1392
                                       (coe (d1916 (coe v0))))))))))))))
name1948 = "Algebra.Ring._.∙-congʳ"
d1948 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1948 v0 v1 v2 = du1948 v2
du1948 ::
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1948 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d484 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d278 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v5))))
name1950 = "Algebra.Ring._.∙-congˡ"
d1950 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1950 v0 v1 v2 = du1950 v2
du1950 ::
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1950 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d484 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d278 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v5))))
name1952 = "Algebra.Ring._.identity"
d1952 :: T1880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1952 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d484
               (coe
                  (MAlonzo.Code.Algebra.Structures.d562
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1392
                           (coe (d1916 (coe v0))))))))))
name1954 = "Algebra.Ring._.identityʳ"
d1954 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d1954 v0 v1 v2 = du1954 v2
du1954 :: T1880 -> AgdaAny -> AgdaAny
du1954 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d484 (coe v3) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v4))))
name1956 = "Algebra.Ring._.identityˡ"
d1956 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d1956 v0 v1 v2 = du1956 v2
du1956 :: T1880 -> AgdaAny -> AgdaAny
du1956 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d484 (coe v3) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v4))))
name1958 = "Algebra.Ring._.+-isAbelianGroup"
d1958 :: T1880 -> MAlonzo.Code.Algebra.Structures.T550
d1958 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))
name1960 = "Algebra.Ring._.isCommutativeMonoid"
d1960 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Algebra.Structures.T314
d1960 v0 v1 v2 = du1960 v2
du1960 :: T1880 -> MAlonzo.Code.Algebra.Structures.T314
du1960 v0
  = let v1 = d1916 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du614
         (coe (MAlonzo.Code.Algebra.Structures.d1392 (coe v1))))
name1962 = "Algebra.Ring._.isGroup"
d1962 :: T1880 -> MAlonzo.Code.Algebra.Structures.T470
d1962 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d562
         (coe
            (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))))
name1964 = "Algebra.Ring._.isMagma"
d1964 :: T1880 -> MAlonzo.Code.Algebra.Structures.T84
d1964 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d562
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1392
                                 (coe (d1916 (coe v0))))))))))))
name1966 = "Algebra.Ring._.isMonoid"
d1966 :: T1880 -> MAlonzo.Code.Algebra.Structures.T268
d1966 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d484
         (coe
            (MAlonzo.Code.Algebra.Structures.d562
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))))))
name1968 = "Algebra.Ring._.isSemigroup"
d1968 :: T1880 -> MAlonzo.Code.Algebra.Structures.T118
d1968 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d484
               (coe
                  (MAlonzo.Code.Algebra.Structures.d562
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1392
                           (coe (d1916 (coe v0))))))))))
name1970 = "Algebra.Ring._.⁻¹-cong"
d1970 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1970 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d488
         (coe
            (MAlonzo.Code.Algebra.Structures.d562
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))))))
name1972 = "Algebra.Ring._.inverse"
d1972 :: T1880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1972 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d486
         (coe
            (MAlonzo.Code.Algebra.Structures.d562
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))))))
name1974 = "Algebra.Ring._.inverseʳ"
d1974 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d1974 v0 v1 v2 = du1974 v2
du1974 :: T1880 -> AgdaAny -> AgdaAny
du1974 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v3))))
name1976 = "Algebra.Ring._.inverseˡ"
d1976 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d1976 v0 v1 v2 = du1976 v2
du1976 :: T1880 -> AgdaAny -> AgdaAny
du1976 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v3))))
name1978 = "Algebra.Ring._.distrib"
d1978 :: T1880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d1978 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1396 (coe (d1916 (coe v0))))
name1980 = "Algebra.Ring._.distribʳ"
d1980 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1980 v0 v1 v2 = du1980 v2
du1980 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1980 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1908 (coe v0) in
    let v3 = d1910 (coe v0) in
    let v4 = d1912 (coe v0) in
    let v5 = d1916 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1480
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v7))))
name1982 = "Algebra.Ring._.distribˡ"
d1982 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1982 v0 v1 v2 = du1982 v2
du1982 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du1982 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1908 (coe v0) in
    let v3 = d1910 (coe v0) in
    let v4 = d1912 (coe v0) in
    let v5 = d1916 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1480
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d1106 (coe v6) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v7))))
name1984 = "Algebra.Ring._.isEquivalence"
d1984 :: T1880 -> MAlonzo.Code.Relation.Binary.Core.T402
d1984 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d562
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1392
                                       (coe (d1916 (coe v0))))))))))))))
name1986 = "Algebra.Ring._.isNearSemiring"
d1986 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Algebra.Structures.T752
d1986 v0 v1 v2 = du1986 v2
du1986 :: T1880 -> MAlonzo.Code.Algebra.Structures.T752
du1986 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1908 (coe v0) in
    let v3 = d1910 (coe v0) in
    let v4 = d1912 (coe v0) in
    let v5 = d1916 (coe v0) in
    let v6
          = MAlonzo.Code.Algebra.Structures.du1480
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du860
         (coe v1) (coe v4)
         (coe (MAlonzo.Code.Algebra.Structures.du1174 (coe v6))))
name1988 = "Algebra.Ring._.isSemiring"
d1988 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Algebra.Structures.T1092
d1988 v0 v1 v2 = du1988 v2
du1988 :: T1880 -> MAlonzo.Code.Algebra.Structures.T1092
du1988 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1480
         (coe (d1906 (coe v0))) (coe (d1908 (coe v0)))
         (coe (d1910 (coe v0))) (coe (d1912 (coe v0)))
         (coe (d1916 (coe v0))))
name1990 = "Algebra.Ring._.isSemiringWithoutAnnihilatingZero"
d1990 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Algebra.Structures.T1002
d1990 v0 v1 v2 = du1990 v2
du1990 :: T1880 -> MAlonzo.Code.Algebra.Structures.T1002
du1990 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1478 (coe (d1916 (coe v0))))
name1992 = "Algebra.Ring._.isSemiringWithoutOne"
d1992 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Algebra.Structures.T826
d1992 v0 v1 v2 = du1992 v2
du1992 :: T1880 -> MAlonzo.Code.Algebra.Structures.T826
du1992 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1908 (coe v0) in
    let v3 = d1910 (coe v0) in
    let v4 = d1912 (coe v0) in
    let v5 = d1916 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1174
         (coe
            (MAlonzo.Code.Algebra.Structures.du1480
               (coe v1) (coe v2) (coe v3) (coe v4) (coe v5))))
name1994 = "Algebra.Ring._.refl"
d1994 :: T1880 -> AgdaAny -> AgdaAny
d1994 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1392
                                             (coe (d1916 (coe v0))))))))))))))))
name1996 = "Algebra.Ring._.reflexive"
d1996 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d1996 v0 v1 v2 = du1996 v2
du1996 ::
  T1880 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du1996 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d484 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d278 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d126 (coe v5) in
    coe
      (\ v7 v8 v9 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v6))) v7)
name1998 = "Algebra.Ring._.setoid"
d1998 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Relation.Binary.T128
d1998 v0 v1 v2 = du1998 v2
du1998 :: T1880 -> MAlonzo.Code.Relation.Binary.T128
du1998 v0
  = let v1 = d1916 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1392 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d562 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d484 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d278 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v5))))
name2000 = "Algebra.Ring._.sym"
d2000 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2000 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1392
                                             (coe (d1916 (coe v0))))))))))))))))
name2002 = "Algebra.Ring._.trans"
d2002 ::
  T1880 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2002 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1392
                                             (coe (d1916 (coe v0))))))))))))))))
name2004 = "Algebra.Ring._.uniqueʳ-⁻¹"
d2004 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2004 v0 v1 v2 = du2004 v2
du2004 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2004 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1910 (coe v0) in
    let v3 = d1912 (coe v0) in
    let v4 = d1916 (coe v0) in
    let v5 = MAlonzo.Code.Algebra.Structures.d1392 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du542
         (coe v1) (coe v3) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe v5))))
name2006 = "Algebra.Ring._.uniqueˡ-⁻¹"
d2006 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2006 v0 v1 v2 = du2006 v2
du2006 :: T1880 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2006 v0
  = let v1 = d1906 (coe v0) in
    let v2 = d1910 (coe v0) in
    let v3 = d1912 (coe v0) in
    let v4 = d1916 (coe v0) in
    let v5 = MAlonzo.Code.Algebra.Structures.d1392 (coe v4) in
    coe
      (MAlonzo.Code.Algebra.Structures.du536
         (coe v1) (coe v3) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe v5))))
name2008 = "Algebra.Ring._.zero"
d2008 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T1880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2008 v0 v1 v2 = du2008 v2
du2008 :: T1880 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du2008 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1476
         (coe (d1906 (coe v0))) (coe (d1908 (coe v0)))
         (coe (d1910 (coe v0))) (coe (d1912 (coe v0)))
         (coe (d1916 (coe v0))))
name2010 = "Algebra.Ring._.zeroʳ"
d2010 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d2010 v0 v1 v2 = du2010 v2
du2010 :: T1880 -> AgdaAny -> AgdaAny
du2010 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1474
         (coe (d1906 (coe v0))) (coe (d1908 (coe v0)))
         (coe (d1910 (coe v0))) (coe (d1912 (coe v0)))
         (coe (d1916 (coe v0))))
name2012 = "Algebra.Ring._.zeroˡ"
d2012 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> AgdaAny -> AgdaAny
d2012 v0 v1 v2 = du2012 v2
du2012 :: T1880 -> AgdaAny -> AgdaAny
du2012 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1472
         (coe (d1906 (coe v0))) (coe (d1908 (coe v0)))
         (coe (d1910 (coe v0))) (coe (d1912 (coe v0)))
         (coe (d1916 (coe v0))))
name2014 = "Algebra.Ring.+-abelianGroup"
d2014 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T774
d2014 v0 v1 v2 = du2014 v2
du2014 :: T1880 -> T774
du2014 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C2963 v3 v4 v5 v6) erased erased
      (d1906 (coe v0)) (d1912 (coe v0)) (d1910 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1392 (coe (d1916 (coe v0))))
name2016 = "Algebra.Ring.semiring"
d2016 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T1542
d2016 v0 v1 v2 = du2016 v2
du2016 :: T1880 -> T1542
du2016 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 v7 -> C6067 v3 v4 v5 v6 v7) erased erased
      (d1906 (coe v0)) (d1908 (coe v0)) (d1912 (coe v0)) (d1914 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du1480
         (coe (d1906 (coe v0))) (coe (d1908 (coe v0)))
         (coe (d1910 (coe v0))) (coe (d1912 (coe v0)))
         (coe (d1916 (coe v0))))
name2020 = "Algebra.Ring._.magma"
d2020 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T28
d2020 v0 v1 v2 = du2020 v2
du2020 :: T1880 -> T28
du2020 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1526 (coe v2) in coe (du122 (coe (du402 (coe v3))))
name2022 = "Algebra.Ring._.*-monoid"
d2022 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T344
d2022 v0 v1 v2 = du2022 v2
du2022 :: T1880 -> T344
du2022 v0
  = let v1 = du2016 (coe v0) in coe (du1526 (coe (du1654 (coe v1))))
name2024 = "Algebra.Ring._.rawMagma"
d2024 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T6
d2024 v0 v1 v2 = du2024 v2
du2024 :: T1880 -> T6
du2024 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1526 (coe v2) in
    let v4 = du402 (coe v3) in coe (du70 (coe (du122 (coe v4))))
name2026 = "Algebra.Ring._.rawMonoid"
d2026 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T318
d2026 v0 v1 v2 = du2026 v2
du2026 :: T1880 -> T318
du2026 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in coe (du404 (coe (du1526 (coe v2))))
name2028 = "Algebra.Ring._.semigroup"
d2028 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T76
d2028 v0 v1 v2 = du2028 v2
du2028 :: T1880 -> T76
du2028 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in coe (du402 (coe (du1526 (coe v2))))
name2030 = "Algebra.Ring._.+-commutativeMonoid"
d2030 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T416
d2030 v0 v1 v2 = du2030 v2
du2030 :: T1880 -> T416
du2030 v0
  = let v1 = du2016 (coe v0) in coe (du1512 (coe (du1654 (coe v1))))
name2032 = "Algebra.Ring._.magma"
d2032 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T28
d2032 v0 v1 v2 = du2032 v2
du2032 :: T1880 -> T28
du2032 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in
    let v4 = du478 (coe v3) in coe (du122 (coe (du402 (coe v4))))
name2034 = "Algebra.Ring._.monoid"
d2034 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T344
d2034 v0 v1 v2 = du2034 v2
du2034 :: T1880 -> T344
du2034 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in coe (du478 (coe (du1512 (coe v2))))
name2036 = "Algebra.Ring._.rawMagma"
d2036 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T6
d2036 v0 v1 v2 = du2036 v2
du2036 :: T1880 -> T6
du2036 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in
    let v4 = du478 (coe v3) in
    let v5 = du402 (coe v4) in coe (du70 (coe (du122 (coe v5))))
name2038 = "Algebra.Ring._.rawMonoid"
d2038 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T318
d2038 v0 v1 v2 = du2038 v2
du2038 :: T1880 -> T318
du2038 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in coe (du404 (coe (du478 (coe v3))))
name2040 = "Algebra.Ring._.semigroup"
d2040 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T76
d2040 v0 v1 v2 = du2040 v2
du2040 :: T1880 -> T76
du2040 v0
  = let v1 = du2016 (coe v0) in
    let v2 = du1654 (coe v1) in
    let v3 = du1512 (coe v2) in coe (du402 (coe (du478 (coe v3))))
name2042 = "Algebra.Ring._.nearSemiring"
d2042 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T1030
d2042 v0 v1 v2 = du2042 v2
du2042 :: T1880 -> T1030
du2042 v0
  = let v1 = du2016 (coe v0) in coe (du1228 (coe (du1680 (coe v1))))
name2044 = "Algebra.Ring._.semiringWithoutAnnihilatingZero"
d2044 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T1414
d2044 v0 v1 v2 = du2044 v2
du2044 :: T1880 -> T1414
du2044 v0 = coe (du1654 (coe (du2016 (coe v0))))
name2046 = "Algebra.Ring._.semiringWithoutOne"
d2046 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T1134
d2046 v0 v1 v2 = du2046 v2
du2046 :: T1880 -> T1134
du2046 v0 = coe (du1680 (coe (du2016 (coe v0))))
name2050 = "Algebra.Ring._.group"
d2050 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T678
d2050 v0 v1 v2 = du2050 v2
du2050 :: T1880 -> T678
du2050 v0 = coe (du858 (coe (du2014 (coe v0))))
name2052 = "Algebra.Ring.rawRing"
d2052 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T1880 -> T1848
d2052 v0 v1 v2 = du2052 v2
du2052 :: T1880 -> T1848
du2052 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 -> C7311 v2 v3 v4 v5 v6) erased
      (d1906 (coe v0)) (d1908 (coe v0)) (d1910 (coe v0)) (d1912 (coe v0))
      (d1914 (coe v0))
name2058 = "Algebra.CommutativeRing"
d2058 a0 a1 = ()
data T2058
  = C8175 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
          AgdaAny MAlonzo.Code.Algebra.Structures.T1502
name2080 = "Algebra.CommutativeRing.Carrier"
d2080 :: T2058 -> ()
d2080 = erased
name2082 = "Algebra.CommutativeRing._≈_"
d2082 :: T2058 -> AgdaAny -> AgdaAny -> ()
d2082 = erased
name2084 = "Algebra.CommutativeRing._+_"
d2084 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny
d2084 v0
  = case coe v0 of
      C8175 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name2086 = "Algebra.CommutativeRing._*_"
d2086 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny
d2086 v0
  = case coe v0 of
      C8175 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name2088 = "Algebra.CommutativeRing.-_"
d2088 :: T2058 -> AgdaAny -> AgdaAny
d2088 v0
  = case coe v0 of
      C8175 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name2090 = "Algebra.CommutativeRing.0#"
d2090 :: T2058 -> AgdaAny
d2090 v0
  = case coe v0 of
      C8175 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name2092 = "Algebra.CommutativeRing.1#"
d2092 :: T2058 -> AgdaAny
d2092 v0
  = case coe v0 of
      C8175 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
name2094 = "Algebra.CommutativeRing.isCommutativeRing"
d2094 :: T2058 -> MAlonzo.Code.Algebra.Structures.T1502
d2094 v0
  = case coe v0 of
      C8175 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
name2098 = "Algebra.CommutativeRing._._-_"
d2098 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny
d2098 v0 v1 v2 = du2098 v2
du2098 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny
du2098 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2088 (coe v0) in coe (\ v3 v4 -> coe v1 v3 (coe v2 v4))
name2100 = "Algebra.CommutativeRing._.assoc"
d2100 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2100 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1394
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1518
                           (coe (d2094 (coe v0))))))))))
name2102 = "Algebra.CommutativeRing._.*-comm"
d2102 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny
d2102 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1520 (coe (d2094 (coe v0))))
name2104 = "Algebra.CommutativeRing._.∙-cong"
d2104 ::
  T2058 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2104 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1394
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1518
                                 (coe (d2094 (coe v0))))))))))))
name2106 = "Algebra.CommutativeRing._.∙-congʳ"
d2106 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2106 v0 v1 v2 = du2106 v2
du2106 ::
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2106 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1394 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name2108 = "Algebra.CommutativeRing._.∙-congˡ"
d2108 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2108 v0 v1 v2 = du2108 v2
du2108 ::
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2108 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1394 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d278 (coe v3) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v4))))
name2110 = "Algebra.CommutativeRing._.identity"
d2110 :: T2058 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2110 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d1394
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))))
name2112 = "Algebra.CommutativeRing._.identityʳ"
d2112 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2112 v0 v1 v2 = du2112 v2
du2112 :: T2058 -> AgdaAny -> AgdaAny
du2112 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1394 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v3))))
name2114 = "Algebra.CommutativeRing._.identityˡ"
d2114 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2114 v0 v1 v2 = du2114 v2
du2114 :: T2058 -> AgdaAny -> AgdaAny
du2114 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1394 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v3))))
name2116 = "Algebra.CommutativeRing._.*-isCommutativeMonoid"
d2116 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T314
d2116 v0 v1 v2 = du2116 v2
du2116 :: T2058 -> MAlonzo.Code.Algebra.Structures.T314
du2116 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1618 (coe (d2094 (coe v0))))
name2118 = "Algebra.CommutativeRing._.isMagma"
d2118 :: T2058 -> MAlonzo.Code.Algebra.Structures.T84
d2118 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1394
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1518
                           (coe (d2094 (coe v0))))))))))
name2120 = "Algebra.CommutativeRing._.*-isMonoid"
d2120 :: T2058 -> MAlonzo.Code.Algebra.Structures.T268
d2120 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1394
         (coe
            (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))
name2122 = "Algebra.CommutativeRing._.isSemigroup"
d2122 :: T2058 -> MAlonzo.Code.Algebra.Structures.T118
d2122 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d1394
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))))
name2124 = "Algebra.CommutativeRing._.assoc"
d2124 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2124 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d128
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d562
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1392
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1518
                                       (coe (d2094 (coe v0))))))))))))))
name2126 = "Algebra.CommutativeRing._.comm"
d2126 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny
d2126 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d564
         (coe
            (MAlonzo.Code.Algebra.Structures.d1392
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))))
name2128 = "Algebra.CommutativeRing._.∙-cong"
d2128 ::
  T2058 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2128 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d94
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d562
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1392
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1518
                                             (coe (d2094 (coe v0))))))))))))))))
name2130 = "Algebra.CommutativeRing._.∙-congʳ"
d2130 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2130 v0 v1 v2 = du2130 v2
du2130 ::
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2130 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d484 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du112
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name2132 = "Algebra.CommutativeRing._.∙-congˡ"
d2132 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2132 v0 v1 v2 = du2132 v2
du2132 ::
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2132 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d484 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du108
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name2134 = "Algebra.CommutativeRing._.identity"
d2134 :: T2058 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2134 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d280
         (coe
            (MAlonzo.Code.Algebra.Structures.d484
               (coe
                  (MAlonzo.Code.Algebra.Structures.d562
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1392
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1518
                                 (coe (d2094 (coe v0))))))))))))
name2136 = "Algebra.CommutativeRing._.identityʳ"
d2136 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2136 v0 v1 v2 = du2136 v2
du2136 :: T2058 -> AgdaAny -> AgdaAny
du2136 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d484 (coe v4) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v5))))
name2138 = "Algebra.CommutativeRing._.identityˡ"
d2138 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2138 v0 v1 v2 = du2138 v2
du2138 :: T2058 -> AgdaAny -> AgdaAny
du2138 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d484 (coe v4) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d280 (coe v5))))
name2140 = "Algebra.CommutativeRing._.+-isAbelianGroup"
d2140 :: T2058 -> MAlonzo.Code.Algebra.Structures.T550
d2140 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1392
         (coe
            (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))
name2142 = "Algebra.CommutativeRing._.isCommutativeMonoid"
d2142 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T314
d2142 v0 v1 v2 = du2142 v2
du2142 :: T2058 -> MAlonzo.Code.Algebra.Structures.T314
du2142 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du614
         (coe (MAlonzo.Code.Algebra.Structures.d1392 (coe v2))))
name2144 = "Algebra.CommutativeRing._.isGroup"
d2144 :: T2058 -> MAlonzo.Code.Algebra.Structures.T470
d2144 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d562
         (coe
            (MAlonzo.Code.Algebra.Structures.d1392
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))))
name2146 = "Algebra.CommutativeRing._.isMagma"
d2146 :: T2058 -> MAlonzo.Code.Algebra.Structures.T84
d2146 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d126
         (coe
            (MAlonzo.Code.Algebra.Structures.d278
               (coe
                  (MAlonzo.Code.Algebra.Structures.d484
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d562
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1392
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1518
                                       (coe (d2094 (coe v0))))))))))))))
name2148 = "Algebra.CommutativeRing._.isMonoid"
d2148 :: T2058 -> MAlonzo.Code.Algebra.Structures.T268
d2148 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d484
         (coe
            (MAlonzo.Code.Algebra.Structures.d562
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1392
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1518
                           (coe (d2094 (coe v0))))))))))
name2150 = "Algebra.CommutativeRing._.isSemigroup"
d2150 :: T2058 -> MAlonzo.Code.Algebra.Structures.T118
d2150 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d278
         (coe
            (MAlonzo.Code.Algebra.Structures.d484
               (coe
                  (MAlonzo.Code.Algebra.Structures.d562
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1392
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d1518
                                 (coe (d2094 (coe v0))))))))))))
name2152 = "Algebra.CommutativeRing._.⁻¹-cong"
d2152 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2152 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d488
         (coe
            (MAlonzo.Code.Algebra.Structures.d562
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1392
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1518
                           (coe (d2094 (coe v0))))))))))
name2154 = "Algebra.CommutativeRing._.inverse"
d2154 :: T2058 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2154 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d486
         (coe
            (MAlonzo.Code.Algebra.Structures.d562
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1392
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1518
                           (coe (d2094 (coe v0))))))))))
name2156 = "Algebra.CommutativeRing._.inverseʳ"
d2156 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2156 v0 v1 v2 = du2156 v2
du2156 :: T2058 -> AgdaAny -> AgdaAny
du2156 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v4))))
name2158 = "Algebra.CommutativeRing._.inverseˡ"
d2158 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2158 v0 v1 v2 = du2158 v2
du2158 :: T2058 -> AgdaAny -> AgdaAny
du2158 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d486 (coe v4))))
name2160 = "Algebra.CommutativeRing._.distrib"
d2160 :: T2058 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2160 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1396
         (coe
            (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))))
name2162 = "Algebra.CommutativeRing._.distribʳ"
d2162 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2162 v0 v1 v2 = du2162 v2
du2162 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2162 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    let v6 = MAlonzo.Code.Algebra.Structures.d1518 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.du1480
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1106 (coe v7) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v8))))
name2164 = "Algebra.CommutativeRing._.distribˡ"
d2164 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2164 v0 v1 v2 = du2164 v2
du2164 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2164 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    let v6 = MAlonzo.Code.Algebra.Structures.d1518 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.du1480
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v6) in
    let v8 = MAlonzo.Code.Algebra.Structures.d1106 (coe v7) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d1022 (coe v8))))
name2166 = "Algebra.CommutativeRing._.isCommutativeSemiring"
d2166 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T1192
d2166 v0 v1 v2 = du2166 v2
du2166 :: T2058 -> MAlonzo.Code.Algebra.Structures.T1192
du2166 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.du1620
         (coe (d2084 (coe v0))) (coe (d2086 (coe v0)))
         (coe (d2088 (coe v0))) (coe (d2090 (coe v0)))
         (coe (d2094 (coe v0))))
name2168
  = "Algebra.CommutativeRing._.isCommutativeSemiringWithoutOne"
d2168 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T914
d2168 v0 v1 v2 = du2168 v2
du2168 :: T2058 -> MAlonzo.Code.Algebra.Structures.T914
du2168 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2092 (coe v0) in
    let v6 = d2094 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1362
         (coe v1) (coe v2) (coe v4) (coe v5)
         (coe
            (MAlonzo.Code.Algebra.Structures.du1620
               (coe v1) (coe v2) (coe v3) (coe v4) (coe v6))))
name2170 = "Algebra.CommutativeRing._.isEquivalence"
d2170 :: T2058 -> MAlonzo.Code.Relation.Binary.Core.T402
d2170 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d92
         (coe
            (MAlonzo.Code.Algebra.Structures.d126
               (coe
                  (MAlonzo.Code.Algebra.Structures.d278
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d484
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d562
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d1392
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1518
                                             (coe (d2094 (coe v0))))))))))))))))
name2172 = "Algebra.CommutativeRing._.isNearSemiring"
d2172 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T752
d2172 v0 v1 v2 = du2172 v2
du2172 :: T2058 -> MAlonzo.Code.Algebra.Structures.T752
du2172 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    let v6 = MAlonzo.Code.Algebra.Structures.d1518 (coe v5) in
    let v7
          = MAlonzo.Code.Algebra.Structures.du1480
              (coe v1) (coe v2) (coe v3) (coe v4) (coe v6) in
    coe
      (MAlonzo.Code.Algebra.Structures.du860
         (coe v1) (coe v4)
         (coe (MAlonzo.Code.Algebra.Structures.du1174 (coe v7))))
name2174 = "Algebra.CommutativeRing._.isRing"
d2174 :: T2058 -> MAlonzo.Code.Algebra.Structures.T1374
d2174 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))
name2176 = "Algebra.CommutativeRing._.isSemiring"
d2176 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T1092
d2176 v0 v1 v2 = du2176 v2
du2176 :: T2058 -> MAlonzo.Code.Algebra.Structures.T1092
du2176 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1480
         (coe v1) (coe v2) (coe v3) (coe v4)
         (coe (MAlonzo.Code.Algebra.Structures.d1518 (coe v5))))
name2178
  = "Algebra.CommutativeRing._.isSemiringWithoutAnnihilatingZero"
d2178 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T1002
d2178 v0 v1 v2 = du2178 v2
du2178 :: T2058 -> MAlonzo.Code.Algebra.Structures.T1002
du2178 v0
  = let v1 = d2094 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1478
         (coe (MAlonzo.Code.Algebra.Structures.d1518 (coe v1))))
name2180 = "Algebra.CommutativeRing._.isSemiringWithoutOne"
d2180 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Algebra.Structures.T826
d2180 v0 v1 v2 = du2180 v2
du2180 :: T2058 -> MAlonzo.Code.Algebra.Structures.T826
du2180 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    let v6 = MAlonzo.Code.Algebra.Structures.d1518 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1174
         (coe
            (MAlonzo.Code.Algebra.Structures.du1480
               (coe v1) (coe v2) (coe v3) (coe v4) (coe v6))))
name2182 = "Algebra.CommutativeRing._.refl"
d2182 :: T2058 -> AgdaAny -> AgdaAny
d2182 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1392
                                             (coe
                                                (MAlonzo.Code.Algebra.Structures.d1518
                                                   (coe (d2094 (coe v0))))))))))))))))))
name2184 = "Algebra.CommutativeRing._.reflexive"
d2184 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d2184 v0 v1 v2 = du2184 v2
du2184 ::
  T2058 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du2184 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d484 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    let v7 = MAlonzo.Code.Algebra.Structures.d126 (coe v6) in
    coe
      (\ v8 v9 v10 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d92 (coe v7))) v8)
name2186 = "Algebra.CommutativeRing._.setoid"
d2186 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Relation.Binary.T128
d2186 v0 v1 v2 = du2186 v2
du2186 :: T2058 -> MAlonzo.Code.Relation.Binary.T128
du2186 v0
  = let v1 = d2094 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1518 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d1392 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d562 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d484 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d278 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du106
         (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v6))))
name2188 = "Algebra.CommutativeRing._.sym"
d2188 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2188 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1392
                                             (coe
                                                (MAlonzo.Code.Algebra.Structures.d1518
                                                   (coe (d2094 (coe v0))))))))))))))))))
name2190 = "Algebra.CommutativeRing._.trans"
d2190 ::
  T2058 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2190 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d92
               (coe
                  (MAlonzo.Code.Algebra.Structures.d126
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d278
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d484
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d562
                                       (coe
                                          (MAlonzo.Code.Algebra.Structures.d1392
                                             (coe
                                                (MAlonzo.Code.Algebra.Structures.d1518
                                                   (coe (d2094 (coe v0))))))))))))))))))
name2192 = "Algebra.CommutativeRing._.uniqueʳ-⁻¹"
d2192 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2192 v0 v1 v2 = du2192 v2
du2192 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2192 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2088 (coe v0) in
    let v3 = d2090 (coe v0) in
    let v4 = d2094 (coe v0) in
    let v5 = MAlonzo.Code.Algebra.Structures.d1518 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d1392 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du542
         (coe v1) (coe v3) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe v6))))
name2194 = "Algebra.CommutativeRing._.uniqueˡ-⁻¹"
d2194 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2194 v0 v1 v2 = du2194 v2
du2194 :: T2058 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2194 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2088 (coe v0) in
    let v3 = d2090 (coe v0) in
    let v4 = d2094 (coe v0) in
    let v5 = MAlonzo.Code.Algebra.Structures.d1518 (coe v4) in
    let v6 = MAlonzo.Code.Algebra.Structures.d1392 (coe v5) in
    coe
      (MAlonzo.Code.Algebra.Structures.du536
         (coe v1) (coe v3) (coe v2)
         (coe (MAlonzo.Code.Algebra.Structures.d562 (coe v6))))
name2196 = "Algebra.CommutativeRing._.zero"
d2196 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2058 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2196 v0 v1 v2 = du2196 v2
du2196 :: T2058 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
du2196 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1476
         (coe v1) (coe v2) (coe v3) (coe v4)
         (coe (MAlonzo.Code.Algebra.Structures.d1518 (coe v5))))
name2198 = "Algebra.CommutativeRing._.zeroʳ"
d2198 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2198 v0 v1 v2 = du2198 v2
du2198 :: T2058 -> AgdaAny -> AgdaAny
du2198 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1474
         (coe v1) (coe v2) (coe v3) (coe v4)
         (coe (MAlonzo.Code.Algebra.Structures.d1518 (coe v5))))
name2200 = "Algebra.CommutativeRing._.zeroˡ"
d2200 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> AgdaAny -> AgdaAny
d2200 v0 v1 v2 = du2200 v2
du2200 :: T2058 -> AgdaAny -> AgdaAny
du2200 v0
  = let v1 = d2084 (coe v0) in
    let v2 = d2086 (coe v0) in
    let v3 = d2088 (coe v0) in
    let v4 = d2090 (coe v0) in
    let v5 = d2094 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du1472
         (coe v1) (coe v2) (coe v3) (coe v4)
         (coe (MAlonzo.Code.Algebra.Structures.d1518 (coe v5))))
name2202 = "Algebra.CommutativeRing.ring"
d2202 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1880
d2202 v0 v1 v2 = du2202 v2
du2202 :: T2058 -> T1880
du2202 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 v7 v8 -> C7407 v3 v4 v5 v6 v7 v8) erased
      erased (d2084 (coe v0)) (d2086 (coe v0)) (d2088 (coe v0))
      (d2090 (coe v0)) (d2092 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1518 (coe (d2094 (coe v0))))
name2204 = "Algebra.CommutativeRing.commutativeSemiring"
d2204 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1690
d2204 v0 v1 v2 = du2204 v2
du2204 :: T2058 -> T1690
du2204 v0
  = coe
      (\ v1 v2 v3 v4 v5 v6 v7 -> C6707 v3 v4 v5 v6 v7) erased erased
      (d2084 (coe v0)) (d2086 (coe v0)) (d2090 (coe v0)) (d2092 (coe v0))
      (MAlonzo.Code.Algebra.Structures.du1620
         (coe (d2084 (coe v0))) (coe (d2086 (coe v0)))
         (coe (d2088 (coe v0))) (coe (d2090 (coe v0)))
         (coe (d2094 (coe v0))))
name2208 = "Algebra.CommutativeRing._.+-abelianGroup"
d2208 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T774
d2208 v0 v1 v2 = du2208 v2
du2208 :: T2058 -> T774
du2208 v0 = coe (du2014 (coe (du2202 (coe v0))))
name2210 = "Algebra.CommutativeRing._.group"
d2210 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T678
d2210 v0 v1 v2 = du2210 v2
du2210 :: T2058 -> T678
du2210 v0
  = let v1 = du2202 (coe v0) in coe (du858 (coe (du2014 (coe v1))))
name2212 = "Algebra.CommutativeRing._.rawRing"
d2212 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1848
d2212 v0 v1 v2 = du2212 v2
du2212 :: T2058 -> T1848
du2212 v0 = coe (du2052 (coe (du2202 (coe v0))))
name2216 = "Algebra.CommutativeRing._.*-commutativeMonoid"
d2216 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T416
d2216 v0 v1 v2 = du2216 v2
du2216 :: T2058 -> T416
du2216 v0 = coe (du1842 (coe (du2204 (coe v0))))
name2218 = "Algebra.CommutativeRing._.magma"
d2218 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T28
d2218 v0 v1 v2 = du2218 v2
du2218 :: T2058 -> T28
du2218 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in
    let v4 = du1526 (coe v3) in coe (du122 (coe (du402 (coe v4))))
name2220 = "Algebra.CommutativeRing._.*-monoid"
d2220 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T344
d2220 v0 v1 v2 = du2220 v2
du2220 :: T2058 -> T344
du2220 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in coe (du1526 (coe (du1654 (coe v2))))
name2222 = "Algebra.CommutativeRing._.rawMagma"
d2222 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T6
d2222 v0 v1 v2 = du2222 v2
du2222 :: T2058 -> T6
du2222 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in
    let v4 = du1526 (coe v3) in
    let v5 = du402 (coe v4) in coe (du70 (coe (du122 (coe v5))))
name2224 = "Algebra.CommutativeRing._.rawMonoid"
d2224 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T318
d2224 v0 v1 v2 = du2224 v2
du2224 :: T2058 -> T318
du2224 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in coe (du404 (coe (du1526 (coe v3))))
name2226 = "Algebra.CommutativeRing._.semigroup"
d2226 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T76
d2226 v0 v1 v2 = du2226 v2
du2226 :: T2058 -> T76
du2226 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in coe (du402 (coe (du1526 (coe v3))))
name2228 = "Algebra.CommutativeRing._.+-commutativeMonoid"
d2228 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T416
d2228 v0 v1 v2 = du2228 v2
du2228 :: T2058 -> T416
du2228 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in coe (du1512 (coe (du1654 (coe v2))))
name2230 = "Algebra.CommutativeRing._.magma"
d2230 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T28
d2230 v0 v1 v2 = du2230 v2
du2230 :: T2058 -> T28
du2230 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in
    let v4 = du1512 (coe v3) in
    let v5 = du478 (coe v4) in coe (du122 (coe (du402 (coe v5))))
name2232 = "Algebra.CommutativeRing._.monoid"
d2232 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T344
d2232 v0 v1 v2 = du2232 v2
du2232 :: T2058 -> T344
du2232 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in coe (du478 (coe (du1512 (coe v3))))
name2234 = "Algebra.CommutativeRing._.rawMagma"
d2234 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T6
d2234 v0 v1 v2 = du2234 v2
du2234 :: T2058 -> T6
du2234 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in
    let v4 = du1512 (coe v3) in
    let v5 = du478 (coe v4) in
    let v6 = du402 (coe v5) in coe (du70 (coe (du122 (coe v6))))
name2236 = "Algebra.CommutativeRing._.rawMonoid"
d2236 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T318
d2236 v0 v1 v2 = du2236 v2
du2236 :: T2058 -> T318
du2236 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in
    let v4 = du1512 (coe v3) in coe (du404 (coe (du478 (coe v4))))
name2238 = "Algebra.CommutativeRing._.semigroup"
d2238 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T76
d2238 v0 v1 v2 = du2238 v2
du2238 :: T2058 -> T76
du2238 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in
    let v3 = du1654 (coe v2) in
    let v4 = du1512 (coe v3) in coe (du402 (coe (du478 (coe v4))))
name2240
  = "Algebra.CommutativeRing._.commutativeSemiringWithoutOne"
d2240 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1254
d2240 v0 v1 v2 = du2240 v2
du2240 :: T2058 -> T1254
du2240 v0 = coe (du1844 (coe (du2204 (coe v0))))
name2242 = "Algebra.CommutativeRing._.nearSemiring"
d2242 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1030
d2242 v0 v1 v2 = du2242 v2
du2242 :: T2058 -> T1030
du2242 v0
  = let v1 = du2204 (coe v0) in
    let v2 = du1808 (coe v1) in coe (du1228 (coe (du1680 (coe v2))))
name2244 = "Algebra.CommutativeRing._.semiring"
d2244 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1542
d2244 v0 v1 v2 = du2244 v2
du2244 :: T2058 -> T1542
du2244 v0 = coe (du1808 (coe (du2204 (coe v0))))
name2246
  = "Algebra.CommutativeRing._.semiringWithoutAnnihilatingZero"
d2246 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1414
d2246 v0 v1 v2 = du2246 v2
du2246 :: T2058 -> T1414
du2246 v0
  = let v1 = du2204 (coe v0) in coe (du1654 (coe (du1808 (coe v1))))
name2248 = "Algebra.CommutativeRing._.semiringWithoutOne"
d2248 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2058 -> T1134
d2248 v0 v1 v2 = du2248 v2
du2248 :: T2058 -> T1134
du2248 v0
  = let v1 = du2204 (coe v0) in coe (du1680 (coe (du1808 (coe v1))))
name2254 = "Algebra.BooleanAlgebra"
d2254 a0 a1 = ()
data T2254
  = C8967 (AgdaAny -> AgdaAny -> AgdaAny)
          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny) AgdaAny
          AgdaAny MAlonzo.Code.Algebra.Structures.T1636
name2276 = "Algebra.BooleanAlgebra.Carrier"
d2276 :: T2254 -> ()
d2276 = erased
name2278 = "Algebra.BooleanAlgebra._≈_"
d2278 :: T2254 -> AgdaAny -> AgdaAny -> ()
d2278 = erased
name2280 = "Algebra.BooleanAlgebra._∨_"
d2280 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny
d2280 v0
  = case coe v0 of
      C8967 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
name2282 = "Algebra.BooleanAlgebra._∧_"
d2282 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny
d2282 v0
  = case coe v0 of
      C8967 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
name2284 = "Algebra.BooleanAlgebra.¬_"
d2284 :: T2254 -> AgdaAny -> AgdaAny
d2284 v0
  = case coe v0 of
      C8967 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
name2286 = "Algebra.BooleanAlgebra.⊤"
d2286 :: T2254 -> AgdaAny
d2286 v0
  = case coe v0 of
      C8967 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
name2288 = "Algebra.BooleanAlgebra.⊥"
d2288 :: T2254 -> AgdaAny
d2288 v0
  = case coe v0 of
      C8967 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
name2290 = "Algebra.BooleanAlgebra.isBooleanAlgebra"
d2290 :: T2254 -> MAlonzo.Code.Algebra.Structures.T1636
d2290 v0
  = case coe v0 of
      C8967 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
name2294 = "Algebra.BooleanAlgebra._.absorptive"
d2294 :: T2254 -> MAlonzo.Code.Agda.Builtin.Sigma.T14
d2294 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d656
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2296 = "Algebra.BooleanAlgebra._.isDistributiveLattice"
d2296 :: T2254 -> MAlonzo.Code.Algebra.Structures.T692
d2296 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))
name2298 = "Algebra.BooleanAlgebra._.isEquivalence"
d2298 :: T2254 -> MAlonzo.Code.Relation.Binary.Core.T402
d2298 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d642
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2300 = "Algebra.BooleanAlgebra._.isLattice"
d2300 :: T2254 -> MAlonzo.Code.Algebra.Structures.T620
d2300 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d702
         (coe
            (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))
name2302 = "Algebra.BooleanAlgebra._.refl"
d2302 :: T2254 -> AgdaAny -> AgdaAny
d2302 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d414
         (coe
            (MAlonzo.Code.Algebra.Structures.d642
               (coe
                  (MAlonzo.Code.Algebra.Structures.d702
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1656
                           (coe (d2290 (coe v0))))))))))
name2304 = "Algebra.BooleanAlgebra._.reflexive"
d2304 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
d2304 v0 v1 v2 = du2304 v2
du2304 ::
  T2254 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T12 -> AgdaAny
du2304 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d702 (coe v2) in
    coe
      (\ v4 v5 v6 ->
         MAlonzo.Code.Relation.Binary.Core.du420
           (coe (MAlonzo.Code.Algebra.Structures.d642 (coe v3))) v4)
name2306 = "Algebra.BooleanAlgebra._.sym"
d2306 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2306 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d416
         (coe
            (MAlonzo.Code.Algebra.Structures.d642
               (coe
                  (MAlonzo.Code.Algebra.Structures.d702
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1656
                           (coe (d2290 (coe v0))))))))))
name2308 = "Algebra.BooleanAlgebra._.trans"
d2308 ::
  T2254 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2308 v0
  = coe
      (MAlonzo.Code.Relation.Binary.Core.d418
         (coe
            (MAlonzo.Code.Algebra.Structures.d642
               (coe
                  (MAlonzo.Code.Algebra.Structures.d702
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d1656
                           (coe (d2290 (coe v0))))))))))
name2310 = "Algebra.BooleanAlgebra._.¬-cong"
d2310 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2310 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1662 (coe (d2290 (coe v0))))
name2312 = "Algebra.BooleanAlgebra._.∧-absorbs-∨"
d2312 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny
d2312 v0 v1 v2 = du2312 v2
du2312 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny
du2312 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d702 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d30
         (coe (MAlonzo.Code.Algebra.Structures.d656 (coe v3))))
name2314 = "Algebra.BooleanAlgebra._.∧-assoc"
d2314 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2314 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d652
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2316 = "Algebra.BooleanAlgebra._.∧-comm"
d2316 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny
d2316 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d650
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2318 = "Algebra.BooleanAlgebra._.∧-complementʳ"
d2318 :: T2254 -> AgdaAny -> AgdaAny
d2318 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1660 (coe (d2290 (coe v0))))
name2320 = "Algebra.BooleanAlgebra._.∧-cong"
d2320 ::
  T2254 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2320 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d654
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2322 = "Algebra.BooleanAlgebra._.∧-congʳ"
d2322 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2322 v0 v1 v2 = du2322 v2
du2322 ::
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2322 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du676
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v2))))
name2324 = "Algebra.BooleanAlgebra._.∧-congˡ"
d2324 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2324 v0 v1 v2 = du2324 v2
du2324 ::
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2324 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du672
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v2))))
name2326 = "Algebra.BooleanAlgebra._.∨-absorbs-∧"
d2326 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny
d2326 v0 v1 v2 = du2326 v2
du2326 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny
du2326 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d702 (coe v2) in
    coe
      (MAlonzo.Code.Agda.Builtin.Sigma.d28
         (coe (MAlonzo.Code.Algebra.Structures.d656 (coe v3))))
name2328 = "Algebra.BooleanAlgebra._.∨-assoc"
d2328 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2328 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d646
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2330 = "Algebra.BooleanAlgebra._.∨-comm"
d2330 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny
d2330 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d644
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2332 = "Algebra.BooleanAlgebra._.∨-complementʳ"
d2332 :: T2254 -> AgdaAny -> AgdaAny
d2332 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d1658 (coe (d2290 (coe v0))))
name2334 = "Algebra.BooleanAlgebra._.∨-cong"
d2334 ::
  T2254 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2334 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d648
         (coe
            (MAlonzo.Code.Algebra.Structures.d702
               (coe
                  (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))))
name2336 = "Algebra.BooleanAlgebra._.∨-congʳ"
d2336 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2336 v0 v1 v2 = du2336 v2
du2336 ::
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2336 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du684
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v2))))
name2338 = "Algebra.BooleanAlgebra._.∨-congˡ"
d2338 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2338 v0 v1 v2 = du2338 v2
du2338 ::
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2338 v0
  = let v1 = d2290 (coe v0) in
    let v2 = MAlonzo.Code.Algebra.Structures.d1656 (coe v1) in
    coe
      (MAlonzo.Code.Algebra.Structures.du680
         (coe (MAlonzo.Code.Algebra.Structures.d702 (coe v2))))
name2340 = "Algebra.BooleanAlgebra._.∨-distribʳ-∧"
d2340 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2340 v0
  = coe
      (MAlonzo.Code.Algebra.Structures.d704
         (coe
            (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))))
name2342 = "Algebra.BooleanAlgebra._.∨-∧-distribʳ"
d2342 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d2342 v0 v1 v2 = du2342 v2
du2342 :: T2254 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du2342 v0
  = let v1 = d2290 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.Structures.du744
         (coe (MAlonzo.Code.Algebra.Structures.d1656 (coe v1))))
name2344 = "Algebra.BooleanAlgebra.distributiveLattice"
d2344 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2254 -> T950
d2344 v0 v1 v2 = du2344 v2
du2344 :: T2254 -> T950
du2344 v0
  = coe
      (\ v1 v2 v3 v4 v5 -> C3739 v3 v4 v5) erased erased (d2280 (coe v0))
      (d2282 (coe v0))
      (MAlonzo.Code.Algebra.Structures.d1656 (coe (d2290 (coe v0))))
name2348 = "Algebra.BooleanAlgebra._.lattice"
d2348 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> T2254 -> T880
d2348 v0 v1 v2 = du2348 v2
du2348 :: T2254 -> T880
du2348 v0 = coe (du1020 (coe (du2344 (coe v0))))
name2350 = "Algebra.BooleanAlgebra._.setoid"
d2350 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  T2254 -> MAlonzo.Code.Relation.Binary.T128
d2350 v0 v1 v2 = du2350 v2
du2350 :: T2254 -> MAlonzo.Code.Relation.Binary.T128
du2350 v0
  = let v1 = du2344 (coe v0) in coe (du944 (coe (du1020 (coe v1))))
name2352 = "Algebra.RawSemigroup"
d2352 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 -> ()
d2352 = erased
