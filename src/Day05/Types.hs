module Types where

newtype Seeds =
    Seeds [Int]
        deriving Show

data Entry =
    Entry { getSource           :: !String
          , getDest             :: !String
          , getDestRangeStart   :: !Int
          , getSourceRangeStart :: !Int
          , getMappings         :: !Int
          } deriving Show

data Contained =
    Contained { getLeft  :: Maybe (Int, Int)
              , getIn    :: Maybe (Int, Int)
              , getRight :: Maybe (Int, Int)
              }

empty :: Contained
empty = Contained Nothing Nothing Nothing