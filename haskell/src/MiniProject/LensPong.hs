{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
module MiniProject.LensPong where


import           Control.Lens                         hiding (at, (:>))
import           Control.Monad                        (when)
import           Control.Monad.State                  (State, execState, get)

import           Data.Conduit                         (ConduitT)

import qualified Data.Conduit                         as C

import           Data.Set                             (Set, empty)

import           Graphics.Gloss                       hiding (display)
import           Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Graphics.Gloss.Interface.Pure.Game

import           System.Random                        (newStdGen, randomRs)


gameSize = 300 :: Float
windowWidth = 800 :: Int
windowHeight = 600 :: Int

ballRadius = 0.02 :: Float
accelearattion = 1.2 :: Float
losingAccuracy = 0.9 :: Float
winningAccuracy = 0.1 :: Float
initialSpeed = 0.6 :: Float
paddleWidth = 0.02 :: Float
paddleHeight = 0.3 ::Float
paddleSpeed = 1 :: Float

textSize = 0.001 :: Float


data Pong =
  Pong { _ballPos   :: Point
       , _ballSpeed :: Vector
       , _paddle1   :: Float
       , _paddle2   :: Float
       , _score     :: (Int, Int)
       , _vectors   :: ConduitT () () Identity Vector
       , _keys      :: Set Key
       }

makeLenses ''Pong

s :: ConduitT () () Identity (Int, Int)
s = return (0, 0)

-- rename tuple length
_x = _1
_y = _2

initial :: Pong
initial = Pong (0, 0) (0, 0) 0 0 (0, 0) (return (0, 0)) empty
