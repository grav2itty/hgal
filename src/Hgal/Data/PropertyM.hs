module Hgal.Data.PropertyM where

import qualified Data.Vector as V

class Property m s k p | s k -> p, s p -> k where
  getProperty :: s -> k -> m (Maybe p)
  adjustProperty :: s -> (p -> p) -> k  -> m ()
  replaceProperty :: s -> k -> p -> m ()
  replaceProperty s k v = adjustProperty s (const v) k
  properties :: s -> m (V.Vector (Maybe p))
