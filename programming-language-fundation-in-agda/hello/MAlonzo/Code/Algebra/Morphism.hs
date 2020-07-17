{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Algebra.Morphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary
import qualified MAlonzo.Code.Relation.Binary.Core
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single

name18 = "Algebra.Morphism.Definitions.Morphism"
d18 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> () -> (AgdaAny -> AgdaAny -> ()) -> ()
d18 = erased
name20 = "Algebra.Morphism.Definitions.Homomorphic₀"
d20 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d20 = erased
name28 = "Algebra.Morphism.Definitions.Homomorphic₁"
d28 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d28 = erased
name38 = "Algebra.Morphism.Definitions.Homomorphic₂"
d38 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d38 = erased
name68 = "Algebra.Morphism._.F._∙_"
d68 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T76 ->
  MAlonzo.Code.Algebra.T76 -> AgdaAny -> AgdaAny -> AgdaAny
d68 v0 v1 v2 v3 v4 v5 = du68 v4
du68 :: MAlonzo.Code.Algebra.T76 -> AgdaAny -> AgdaAny -> AgdaAny
du68 v0 = coe (MAlonzo.Code.Algebra.d94 (coe v0))
name70 = "Algebra.Morphism._.F._≈_"
d70 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T76 ->
  MAlonzo.Code.Algebra.T76 -> AgdaAny -> AgdaAny -> ()
d70 = erased
name140 = "Algebra.Morphism._._.Homomorphic₀"
d140 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T76 ->
  MAlonzo.Code.Algebra.T76 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d140 = erased
name142 = "Algebra.Morphism._._.Homomorphic₁"
d142 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T76 ->
  MAlonzo.Code.Algebra.T76 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d142 = erased
name144 = "Algebra.Morphism._._.Homomorphic₂"
d144 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T76 ->
  MAlonzo.Code.Algebra.T76 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d144 = erased
name146 = "Algebra.Morphism._._.Morphism"
d146 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T76 -> MAlonzo.Code.Algebra.T76 -> ()
d146 = erased
name150 = "Algebra.Morphism._.IsSemigroupMorphism"
d150 a0 a1 a2 a3 a4 a5 a6 = ()
data T150
  = C593 (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
         (AgdaAny -> AgdaAny -> AgdaAny)
name158 = "Algebra.Morphism._.IsSemigroupMorphism.⟦⟧-cong"
d158 :: T150 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d158 v0
  = case coe v0 of
      C593 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name160 = "Algebra.Morphism._.IsSemigroupMorphism.∙-homo"
d160 :: T150 -> AgdaAny -> AgdaAny -> AgdaAny
d160 v0
  = case coe v0 of
      C593 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name212 = "Algebra.Morphism._.F.semigroup"
d212 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 ->
  MAlonzo.Code.Algebra.T344 -> MAlonzo.Code.Algebra.T76
d212 v0 v1 v2 v3 v4 v5 = du212 v4
du212 :: MAlonzo.Code.Algebra.T344 -> MAlonzo.Code.Algebra.T76
du212 v0 = coe (MAlonzo.Code.Algebra.du402 (coe v0))
name220 = "Algebra.Morphism._.F.ε"
d220 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 -> MAlonzo.Code.Algebra.T344 -> AgdaAny
d220 v0 v1 v2 v3 v4 v5 = du220 v4
du220 :: MAlonzo.Code.Algebra.T344 -> AgdaAny
du220 v0 = coe (MAlonzo.Code.Algebra.d366 (coe v0))
name262 = "Algebra.Morphism._.T.semigroup"
d262 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 ->
  MAlonzo.Code.Algebra.T344 -> MAlonzo.Code.Algebra.T76
d262 v0 v1 v2 v3 v4 v5 = du262 v5
du262 :: MAlonzo.Code.Algebra.T344 -> MAlonzo.Code.Algebra.T76
du262 v0 = coe (MAlonzo.Code.Algebra.du402 (coe v0))
name280 = "Algebra.Morphism._._.Homomorphic₀"
d280 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 ->
  MAlonzo.Code.Algebra.T344 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d280 = erased
name282 = "Algebra.Morphism._._.Homomorphic₁"
d282 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 ->
  MAlonzo.Code.Algebra.T344 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d282 = erased
name284 = "Algebra.Morphism._._.Homomorphic₂"
d284 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 ->
  MAlonzo.Code.Algebra.T344 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d284 = erased
name286 = "Algebra.Morphism._._.Morphism"
d286 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T344 -> MAlonzo.Code.Algebra.T344 -> ()
d286 = erased
name290 = "Algebra.Morphism._.IsMonoidMorphism"
d290 a0 a1 a2 a3 a4 a5 a6 = ()
data T290 = C881 T150 AgdaAny
name298 = "Algebra.Morphism._.IsMonoidMorphism.sm-homo"
d298 :: T290 -> T150
d298 v0
  = case coe v0 of
      C881 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name300 = "Algebra.Morphism._.IsMonoidMorphism.ε-homo"
d300 :: T290 -> AgdaAny
d300 v0
  = case coe v0 of
      C881 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
name304 = "Algebra.Morphism._.IsMonoidMorphism._.∙-homo"
d304 :: T290 -> AgdaAny -> AgdaAny -> AgdaAny
d304 v0 = coe (d160 (coe (d298 (coe v0))))
name306 = "Algebra.Morphism._.IsMonoidMorphism._.⟦⟧-cong"
d306 :: T290 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d306 v0 = coe (d158 (coe (d298 (coe v0))))
name354 = "Algebra.Morphism._.F.monoid"
d354 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T416 ->
  MAlonzo.Code.Algebra.T416 -> MAlonzo.Code.Algebra.T344
d354 v0 v1 v2 v3 v4 v5 = du354 v4
du354 :: MAlonzo.Code.Algebra.T416 -> MAlonzo.Code.Algebra.T344
du354 v0 = coe (MAlonzo.Code.Algebra.du478 (coe v0))
name410 = "Algebra.Morphism._.T.monoid"
d410 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T416 ->
  MAlonzo.Code.Algebra.T416 -> MAlonzo.Code.Algebra.T344
d410 v0 v1 v2 v3 v4 v5 = du410 v5
du410 :: MAlonzo.Code.Algebra.T416 -> MAlonzo.Code.Algebra.T344
du410 v0 = coe (MAlonzo.Code.Algebra.du478 (coe v0))
name438 = "Algebra.Morphism._._.Homomorphic₀"
d438 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T416 ->
  MAlonzo.Code.Algebra.T416 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d438 = erased
name440 = "Algebra.Morphism._._.Homomorphic₁"
d440 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T416 ->
  MAlonzo.Code.Algebra.T416 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d440 = erased
name442 = "Algebra.Morphism._._.Homomorphic₂"
d442 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T416 ->
  MAlonzo.Code.Algebra.T416 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d442 = erased
name444 = "Algebra.Morphism._._.Morphism"
d444 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T416 -> MAlonzo.Code.Algebra.T416 -> ()
d444 = erased
name448 = "Algebra.Morphism._.IsCommutativeMonoidMorphism"
d448 a0 a1 a2 a3 a4 a5 a6 = ()
newtype T448 = C1239 T290
name454 = "Algebra.Morphism._.IsCommutativeMonoidMorphism.mn-homo"
d454 :: T448 -> T290
d454 v0
  = case coe v0 of
      C1239 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name458
  = "Algebra.Morphism._.IsCommutativeMonoidMorphism._.sm-homo"
d458 :: T448 -> T150
d458 v0 = coe (d298 (coe (d454 (coe v0))))
name460 = "Algebra.Morphism._.IsCommutativeMonoidMorphism._.ε-homo"
d460 :: T448 -> AgdaAny
d460 v0 = coe (d300 (coe (d454 (coe v0))))
name462 = "Algebra.Morphism._.IsCommutativeMonoidMorphism._.∙-homo"
d462 :: T448 -> AgdaAny -> AgdaAny -> AgdaAny
d462 v0 = coe (d160 (coe (d298 (coe (d454 (coe v0))))))
name464
  = "Algebra.Morphism._.IsCommutativeMonoidMorphism._.⟦⟧-cong"
d464 :: T448 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d464 v0 = coe (d158 (coe (d298 (coe (d454 (coe v0))))))
name494 = "Algebra.Morphism._.F.commutativeMonoid"
d494 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T416
d494 v0 v1 v2 v3 v4 v5 = du494 v4
du494 :: MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T416
du494 v0 = coe (MAlonzo.Code.Algebra.du560 (coe v0))
name518 = "Algebra.Morphism._.F.monoid"
d518 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T344
d518 v0 v1 v2 v3 v4 v5 = du518 v4
du518 :: MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T344
du518 v0
  = coe
      (MAlonzo.Code.Algebra.du478
         (coe (MAlonzo.Code.Algebra.du560 (coe v0))))
name556 = "Algebra.Morphism._.T.commutativeMonoid"
d556 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T416
d556 v0 v1 v2 v3 v4 v5 = du556 v5
du556 :: MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T416
du556 v0 = coe (MAlonzo.Code.Algebra.du560 (coe v0))
name580 = "Algebra.Morphism._.T.monoid"
d580 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T344
d580 v0 v1 v2 v3 v4 v5 = du580 v5
du580 :: MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T344
du580 v0
  = coe
      (MAlonzo.Code.Algebra.du478
         (coe (MAlonzo.Code.Algebra.du560 (coe v0))))
name608 = "Algebra.Morphism._._.Homomorphic₀"
d608 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d608 = erased
name610 = "Algebra.Morphism._._.Homomorphic₁"
d610 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d610 = erased
name612 = "Algebra.Morphism._._.Homomorphic₂"
d612 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d612 = erased
name614 = "Algebra.Morphism._._.Morphism"
d614 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 -> MAlonzo.Code.Algebra.T494 -> ()
d614 = erased
name618
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism"
d618 a0 a1 a2 a3 a4 a5 a6 = ()
newtype T618 = C1647 T290
name624
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism.mn-homo"
d624 :: T618 -> T290
d624 v0
  = case coe v0 of
      C1647 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name628
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism._.sm-homo"
d628 :: T618 -> T150
d628 v0 = coe (d298 (coe (d624 (coe v0))))
name630
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism._.ε-homo"
d630 :: T618 -> AgdaAny
d630 v0 = coe (d300 (coe (d624 (coe v0))))
name632
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism._.∙-homo"
d632 :: T618 -> AgdaAny -> AgdaAny -> AgdaAny
d632 v0 = coe (d160 (coe (d298 (coe (d624 (coe v0))))))
name634
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism._.⟦⟧-cong"
d634 :: T618 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d634 v0 = coe (d158 (coe (d298 (coe (d624 (coe v0))))))
name636
  = "Algebra.Morphism._.IsIdempotentCommutativeMonoidMorphism.isCommutativeMonoidMorphism"
d636 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T494 ->
  MAlonzo.Code.Algebra.T494 -> (AgdaAny -> AgdaAny) -> T618 -> T448
d636 v0 v1 v2 v3 v4 v5 v6 v7 = du636 v7
du636 :: T618 -> T448
du636 v0 = coe (C1239 (coe (d624 (coe v0))))
name658 = "Algebra.Morphism._.F._⁻¹"
d658 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 -> AgdaAny -> AgdaAny
d658 v0 v1 v2 v3 v4 v5 = du658 v4
du658 :: MAlonzo.Code.Algebra.T678 -> AgdaAny -> AgdaAny
du658 v0 = coe (MAlonzo.Code.Algebra.d704 (coe v0))
name692 = "Algebra.Morphism._.F.monoid"
d692 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 -> MAlonzo.Code.Algebra.T344
d692 v0 v1 v2 v3 v4 v5 = du692 v4
du692 :: MAlonzo.Code.Algebra.T678 -> MAlonzo.Code.Algebra.T344
du692 v0 = coe (MAlonzo.Code.Algebra.du758 (coe v0))
name764 = "Algebra.Morphism._.T.monoid"
d764 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 -> MAlonzo.Code.Algebra.T344
d764 v0 v1 v2 v3 v4 v5 = du764 v5
du764 :: MAlonzo.Code.Algebra.T678 -> MAlonzo.Code.Algebra.T344
du764 v0 = coe (MAlonzo.Code.Algebra.du758 (coe v0))
name800 = "Algebra.Morphism._._.Homomorphic₀"
d800 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d800 = erased
name802 = "Algebra.Morphism._._.Homomorphic₁"
d802 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d802 = erased
name804 = "Algebra.Morphism._._.Homomorphic₂"
d804 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d804 = erased
name806 = "Algebra.Morphism._._.Morphism"
d806 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 -> MAlonzo.Code.Algebra.T678 -> ()
d806 = erased
name810 = "Algebra.Morphism._.IsGroupMorphism"
d810 a0 a1 a2 a3 a4 a5 a6 = ()
newtype T810 = C2267 T290
name816 = "Algebra.Morphism._.IsGroupMorphism.mn-homo"
d816 :: T810 -> T290
d816 v0
  = case coe v0 of
      C2267 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name820 = "Algebra.Morphism._.IsGroupMorphism._.sm-homo"
d820 :: T810 -> T150
d820 v0 = coe (d298 (coe (d816 (coe v0))))
name822 = "Algebra.Morphism._.IsGroupMorphism._.ε-homo"
d822 :: T810 -> AgdaAny
d822 v0 = coe (d300 (coe (d816 (coe v0))))
name824 = "Algebra.Morphism._.IsGroupMorphism._.∙-homo"
d824 :: T810 -> AgdaAny -> AgdaAny -> AgdaAny
d824 v0 = coe (d160 (coe (d298 (coe (d816 (coe v0))))))
name826 = "Algebra.Morphism._.IsGroupMorphism._.⟦⟧-cong"
d826 :: T810 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d826 v0 = coe (d158 (coe (d298 (coe (d816 (coe v0))))))
name828 = "Algebra.Morphism._.IsGroupMorphism.⁻¹-homo"
d828 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 ->
  (AgdaAny -> AgdaAny) -> T810 -> AgdaAny -> AgdaAny
d828 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du828 v4 v5 v6 v7 v8
du828 ::
  MAlonzo.Code.Algebra.T678 ->
  MAlonzo.Code.Algebra.T678 ->
  (AgdaAny -> AgdaAny) -> T810 -> AgdaAny -> AgdaAny
du828 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.du536
      (MAlonzo.Code.Algebra.d700 (coe v1))
      (MAlonzo.Code.Algebra.d702 (coe v1))
      (MAlonzo.Code.Algebra.d704 (coe v1))
      (MAlonzo.Code.Algebra.d706 (coe v1))
      (coe v2 (coe MAlonzo.Code.Algebra.d704 v0 v4)) (coe v2 v4)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d40
         (coe
            (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du50
               (coe
                  (MAlonzo.Code.Relation.Binary.Core.d418
                     (coe
                        (MAlonzo.Code.Relation.Binary.d144
                           (let v5 = MAlonzo.Code.Algebra.d706 (coe v1) in
                            let v6 = MAlonzo.Code.Algebra.Structures.d484 (coe v5) in
                            let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
                            coe
                              (MAlonzo.Code.Algebra.Structures.du106
                                 (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v7)))))))))
               (coe
                  MAlonzo.Code.Algebra.d700 v1
                  (coe v2 (coe MAlonzo.Code.Algebra.d704 v0 v4)) (coe v2 v4))
               (coe
                  v2
                  (let v5
                         = let v5 = MAlonzo.Code.Algebra.du758 (coe v0) in
                           MAlonzo.Code.Algebra.du402 (coe v5) in
                   coe
                     MAlonzo.Code.Algebra.d94 v5 (coe MAlonzo.Code.Algebra.d704 v0 v4)
                     v4))
               (coe (MAlonzo.Code.Algebra.d702 (coe v1)))
               (coe
                  MAlonzo.Code.Relation.Binary.Core.d416
                  (MAlonzo.Code.Algebra.Structures.d92
                     (coe
                        (MAlonzo.Code.Algebra.Structures.d126
                           (coe
                              (MAlonzo.Code.Algebra.Structures.d278
                                 (coe
                                    (MAlonzo.Code.Algebra.Structures.d484
                                       (coe (MAlonzo.Code.Algebra.d706 (coe v1))))))))))
                  (coe
                     v2
                     (let v5
                            = let v5 = MAlonzo.Code.Algebra.du758 (coe v0) in
                              MAlonzo.Code.Algebra.du402 (coe v5) in
                      coe
                        MAlonzo.Code.Algebra.d94 v5 (coe MAlonzo.Code.Algebra.d704 v0 v4)
                        v4))
                  (coe
                     MAlonzo.Code.Algebra.d94
                     (let v5 = MAlonzo.Code.Algebra.du758 (coe v1) in
                      MAlonzo.Code.Algebra.du402 (coe v5))
                     (coe v2 (coe MAlonzo.Code.Algebra.d704 v0 v4)) (coe v2 v4))
                  (coe
                     d160 (d298 (coe (d816 (coe v3))))
                     (coe MAlonzo.Code.Algebra.d704 v0 v4) v4))
               (coe
                  (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du50
                     (coe
                        (MAlonzo.Code.Relation.Binary.Core.d418
                           (coe
                              (MAlonzo.Code.Relation.Binary.d144
                                 (let v5 = MAlonzo.Code.Algebra.d706 (coe v1) in
                                  let v6 = MAlonzo.Code.Algebra.Structures.d484 (coe v5) in
                                  let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
                                  coe
                                    (MAlonzo.Code.Algebra.Structures.du106
                                       (coe (MAlonzo.Code.Algebra.Structures.d126 (coe v7)))))))))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.d700 v0 (coe MAlonzo.Code.Algebra.d704 v0 v4)
                           v4))
                     (coe v2 (MAlonzo.Code.Algebra.d702 (coe v0)))
                     (coe (MAlonzo.Code.Algebra.d702 (coe v1)))
                     (coe
                        d158 (d298 (coe (d816 (coe v3))))
                        (coe
                           MAlonzo.Code.Algebra.d700 v0 (coe MAlonzo.Code.Algebra.d704 v0 v4)
                           v4)
                        (MAlonzo.Code.Algebra.d702 (coe v0))
                        (let v5 = MAlonzo.Code.Algebra.d706 (coe v0) in
                         coe
                           MAlonzo.Code.Agda.Builtin.Sigma.d28
                           (MAlonzo.Code.Algebra.Structures.d486 (coe v5)) v4))
                     (coe
                        (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du50
                           (coe
                              (MAlonzo.Code.Relation.Binary.Core.d418
                                 (coe
                                    (MAlonzo.Code.Relation.Binary.d144
                                       (let v5 = MAlonzo.Code.Algebra.d706 (coe v1) in
                                        let v6 = MAlonzo.Code.Algebra.Structures.d484 (coe v5) in
                                        let v7 = MAlonzo.Code.Algebra.Structures.d278 (coe v6) in
                                        coe
                                          (MAlonzo.Code.Algebra.Structures.du106
                                             (coe
                                                (MAlonzo.Code.Algebra.Structures.d126
                                                   (coe v7)))))))))
                           (coe v2 (MAlonzo.Code.Algebra.d702 (coe v0)))
                           (coe
                              (MAlonzo.Code.Algebra.d366
                                 (coe (MAlonzo.Code.Algebra.du758 (coe v1)))))
                           (coe (MAlonzo.Code.Algebra.d702 (coe v1)))
                           (coe (d300 (coe (d816 (coe v3)))))
                           (let v5
                                  = MAlonzo.Code.Relation.Binary.Core.d414
                                      (coe
                                         (MAlonzo.Code.Relation.Binary.d144
                                            (let v5 = MAlonzo.Code.Algebra.d706 (coe v1) in
                                             let v6
                                                   = MAlonzo.Code.Algebra.Structures.d484
                                                       (coe v5) in
                                             let v7
                                                   = MAlonzo.Code.Algebra.Structures.d278
                                                       (coe v6) in
                                             coe
                                               (MAlonzo.Code.Algebra.Structures.du106
                                                  (coe
                                                     (MAlonzo.Code.Algebra.Structures.d126
                                                        (coe v7))))))) in
                            let v6 = MAlonzo.Code.Algebra.d702 (coe v1) in
                            coe
                              (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.C34
                                 (coe v5 v6))))))))))
name890 = "Algebra.Morphism._.F.group"
d890 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 -> MAlonzo.Code.Algebra.T678
d890 v0 v1 v2 v3 v4 v5 = du890 v4
du890 :: MAlonzo.Code.Algebra.T774 -> MAlonzo.Code.Algebra.T678
du890 v0 = coe (MAlonzo.Code.Algebra.du858 (coe v0))
name972 = "Algebra.Morphism._.T.group"
d972 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 -> MAlonzo.Code.Algebra.T678
d972 v0 v1 v2 v3 v4 v5 = du972 v5
du972 :: MAlonzo.Code.Algebra.T774 -> MAlonzo.Code.Algebra.T678
du972 v0 = coe (MAlonzo.Code.Algebra.du858 (coe v0))
name1038 = "Algebra.Morphism._._.Homomorphic₀"
d1038 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d1038 = erased
name1040 = "Algebra.Morphism._._.Homomorphic₁"
d1040 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d1040 = erased
name1042 = "Algebra.Morphism._._.Homomorphic₂"
d1042 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d1042 = erased
name1044 = "Algebra.Morphism._._.Morphism"
d1044 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 -> MAlonzo.Code.Algebra.T774 -> ()
d1044 = erased
name1048 = "Algebra.Morphism._.IsAbelianGroupMorphism"
d1048 a0 a1 a2 a3 a4 a5 a6 = ()
newtype T1048 = C3337 T810
name1054 = "Algebra.Morphism._.IsAbelianGroupMorphism.gp-homo"
d1054 :: T1048 -> T810
d1054 v0
  = case coe v0 of
      C3337 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name1058 = "Algebra.Morphism._.IsAbelianGroupMorphism._.mn-homo"
d1058 :: T1048 -> T290
d1058 v0 = coe (d816 (coe (d1054 (coe v0))))
name1060 = "Algebra.Morphism._.IsAbelianGroupMorphism._.sm-homo"
d1060 :: T1048 -> T150
d1060 v0 = coe (d298 (coe (d816 (coe (d1054 (coe v0))))))
name1062 = "Algebra.Morphism._.IsAbelianGroupMorphism._.ε-homo"
d1062 :: T1048 -> AgdaAny
d1062 v0 = coe (d300 (coe (d816 (coe (d1054 (coe v0))))))
name1064 = "Algebra.Morphism._.IsAbelianGroupMorphism._.⁻¹-homo"
d1064 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 ->
  (AgdaAny -> AgdaAny) -> T1048 -> AgdaAny -> AgdaAny
d1064 v0 v1 v2 v3 v4 v5 v6 v7 = du1064 v4 v5 v6 v7
du1064 ::
  MAlonzo.Code.Algebra.T774 ->
  MAlonzo.Code.Algebra.T774 ->
  (AgdaAny -> AgdaAny) -> T1048 -> AgdaAny -> AgdaAny
du1064 v0 v1 v2 v3
  = coe
      (du828
         (coe (MAlonzo.Code.Algebra.du858 (coe v0)))
         (coe (MAlonzo.Code.Algebra.du858 (coe v1))) (coe v2)
         (coe (d1054 (coe v3))))
name1066 = "Algebra.Morphism._.IsAbelianGroupMorphism._.∙-homo"
d1066 :: T1048 -> AgdaAny -> AgdaAny -> AgdaAny
d1066 v0
  = coe (d160 (coe (d298 (coe (d816 (coe (d1054 (coe v0))))))))
name1068 = "Algebra.Morphism._.IsAbelianGroupMorphism._.⟦⟧-cong"
d1068 :: T1048 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d1068 v0
  = coe (d158 (coe (d298 (coe (d816 (coe (d1054 (coe v0))))))))
name1118 = "Algebra.Morphism._.F.*-monoid"
d1118 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T344
d1118 v0 v1 v2 v3 v4 v5 = du1118 v4
du1118 :: MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T344
du1118 v0
  = let v1 = MAlonzo.Code.Algebra.du2016 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.du1526
         (coe (MAlonzo.Code.Algebra.du1654 (coe v1))))
name1126 = "Algebra.Morphism._.F.+-abelianGroup"
d1126 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T774
d1126 v0 v1 v2 v3 v4 v5 = du1126 v4
du1126 :: MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T774
du1126 v0 = coe (MAlonzo.Code.Algebra.du2014 (coe v0))
name1266 = "Algebra.Morphism._.T.*-monoid"
d1266 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T344
d1266 v0 v1 v2 v3 v4 v5 = du1266 v5
du1266 :: MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T344
du1266 v0
  = let v1 = MAlonzo.Code.Algebra.du2016 (coe v0) in
    coe
      (MAlonzo.Code.Algebra.du1526
         (coe (MAlonzo.Code.Algebra.du1654 (coe v1))))
name1274 = "Algebra.Morphism._.T.+-abelianGroup"
d1274 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T774
d1274 v0 v1 v2 v3 v4 v5 = du1274 v5
du1274 :: MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T774
du1274 v0 = coe (MAlonzo.Code.Algebra.du2014 (coe v0))
name1384 = "Algebra.Morphism._._.Homomorphic₀"
d1384 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d1384 = erased
name1386 = "Algebra.Morphism._._.Homomorphic₁"
d1386 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d1386 = erased
name1388 = "Algebra.Morphism._._.Homomorphic₂"
d1388 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 ->
  MAlonzo.Code.Algebra.T1880 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d1388 = erased
name1390 = "Algebra.Morphism._._.Morphism"
d1390 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Algebra.T1880 -> MAlonzo.Code.Algebra.T1880 -> ()
d1390 = erased
name1394 = "Algebra.Morphism._.IsRingMorphism"
d1394 a0 a1 a2 a3 a4 a5 a6 = ()
data T1394 = C3987 T1048 T290
name1402 = "Algebra.Morphism._.IsRingMorphism.+-abgp-homo"
d1402 :: T1394 -> T1048
d1402 v0
  = case coe v0 of
      C3987 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
name1404 = "Algebra.Morphism._.IsRingMorphism.*-mn-homo"
d1404 :: T1394 -> T290
d1404 v0
  = case coe v0 of
      C3987 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
