{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
module Types.ExSubTyping where

-- mimic subtyping with typeclass constraint and existential type.
-- need more work to simulate shared state though.
class WidgetC a where
  wHeight :: a -> Double
  wWidth :: a -> Double

class WidgetC a => LabelWidgetC a where
  lblName :: a -> String

-- define some extensial types.
data W = forall a. (WidgetC a) => W a
data LabelW = forall a. LabelWidgetC a => LabelW a

-- implemnet instances
data Widget = Widget { w :: Double, h :: Double }
data LabelWidget = LabelWidget
  { lw        :: Double
  , lh        :: Double
  , labelName :: String
  }

-- for wrapper types -->
instance WidgetC W where
  wHeight (W w) = wHeight w
  wWidth (W w) = wWidth w

instance WidgetC LabelW where
  wHeight (LabelW w) = wHeight w
  wWidth (LabelW w) = wWidth w

instance LabelWidgetC LabelW where
  lblName (LabelW w) = lblName w
-- <---

-- for concrete types --->
instance WidgetC Widget where
  wHeight widget = h widget
  wWidth widget = w widget

instance WidgetC LabelWidget where
  wHeight lbl =  lh lbl
  wWidth lbl = lw lbl

instance LabelWidgetC LabelWidget where
  lblName lbl = labelName lbl
-- <---

getSize ::  W -> (Double, Double)
getSize (W w) = (wWidth w, wHeight w)

a = getSize (W $ Widget 10 10)
b = getSize (W $ LabelWidget 10 30 "label_1")
-- a (10.0, 10.0), b (10.0, 30.0)


-- dynamic dispatch  --


