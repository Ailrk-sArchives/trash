{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Notes.All.Fundeps where

class Extract container elem | container -> elem where
  extract :: container -> elem

instance Extract (a, b) a where
  extract (x, _) = x


