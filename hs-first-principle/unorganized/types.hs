module Types where

data Mood = Blah | Woot deriving Show

-- pattern matching.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
