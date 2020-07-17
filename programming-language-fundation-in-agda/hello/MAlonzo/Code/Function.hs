{-# LANGUAGE EmptyDataDecls, EmptyCase, ExistentialQuantification,
             ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types,
             PatternSynonyms #-}
module MAlonzo.Code.Function where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, eqFloat, add64, sub64,
                    mul64, quot64, rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

name22 = "Function.Fun₁"
d22 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> ()
d22 = erased
name26 = "Function.Fun₂"
d26 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> ()
d26 = erased
name30 = "Function.id"
d30 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> AgdaAny
d30 v0 v1 v2 = du30 v2
du30 :: AgdaAny -> AgdaAny
du30 v0 = coe v0
name34 = "Function.const"
d34 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d34 v0 v1 v2 v3 v4 v5 = du34 v4
du34 :: AgdaAny -> AgdaAny
du34 v0 = coe v0
name58 = "Function._∘_"
d58 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d58 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du58 v6 v7 v8
du58 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du58 v0 v1 v2 = coe v0 v2 (coe v1 v2)
name80 = "Function.flip"
d80 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d80 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du80 v6 v7 v8
du80 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du80 v0 v1 v2 = coe v0 v2 v1
name96 = "Function._$_"
d96 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d96 v0 v1 v2 v3 v4 v5 = du96 v4 v5
du96 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du96 v0 v1 = coe v0 v1
name110 = "Function._$!_"
d110 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d110 v0 v1 v2 v3 v4 v5 = du110 v4 v5
du110 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du110 v0 v1 = coe (seq (coe v1) (coe v0 v1))
name120 = "Function._|>_"
d120 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d120 v0 v1 v2 v3 v4 v5 = du120 v4 v5
du120 :: AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du120 v0 v1 = coe v1 v0
name140 = "Function._ˢ_"
d140 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d140 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du140 v6 v7 v8
du140 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du140 v0 v1 v2 = coe v0 v2 (coe v1 v2)
name156 = "Function.case_return_of_"
d156 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> AgdaAny
d156 v0 v1 v2 v3 v4 v5 = du156 v3 v5
du156 :: AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du156 v0 v1 = coe v1 v0
name172 = "Function._$-"
d172 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d172 v0 v1 v2 v3 v4 v5 = du172 v4 v5
du172 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du172 v0 v1 = coe v0 v1
name184 = "Function.λ-"
d184 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d184 v0 v1 v2 v3 v4 v5 = du184 v4 v5
du184 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du184 v0 v1 = coe v0 v1
name190 = "Function._∘′_"
d190 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d190 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du190 v6 v7 v8
du190 ::
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du190 v0 v1 v2 = coe v0 (coe v1 v2)
name196 = "Function._$′_"
d196 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d196 v0 v1 v2 v3 v4 = du196 v4
du196 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du196 v0 = coe v0
name198 = "Function._$!′_"
d198 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d198 v0 v1 v2 v3 = du198
du198 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du198 = coe du110
name200 = "Function._|>′_"
d200 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d200 v0 v1 v2 v3 v4 v5 = du200 v4 v5
du200 :: AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du200 v0 v1 = coe v1 v0
name202 = "Function.case_of_"
d202 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d202 v0 v1 v2 v3 v4 v5 = du202 v4 v5
du202 :: AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du202 v0 v1 = coe v1 v0
name208 = "Function._⟨_⟩_"
d208 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d208 v0 v1 v2 v3 v4 v5 v6 v7 v8 = du208 v6 v7 v8
du208 ::
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du208 v0 v1 v2 = coe v1 v0 v2
name216 = "Function._on_"
d216 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d216 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = du216 v6 v7 v8 v9
du216 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du216 v0 v1 v2 v3 = coe v0 (coe v1 v2) (coe v1 v3)
name226 = "Function._-[_]-_"
d226 ::
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T4 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d226 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  = du226 v10 v11 v12 v13 v14
du226 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du226 v0 v1 v2 v3 v4 = coe v1 (coe v0 v3 v4) (coe v2 v3 v4)
name240 = "Function._∋_"
d240 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> AgdaAny
d240 v0 v1 v2 = du240 v2
du240 :: AgdaAny -> AgdaAny
du240 v0 = coe v0
name248 = "Function.typeOf"
d248 :: MAlonzo.Code.Agda.Primitive.T4 -> () -> AgdaAny -> ()
d248 = erased
