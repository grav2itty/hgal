module Hgal.Data.Property where

import Control.Lens
import qualified Data.Vector as V

class Property s k p | s k -> p, s p -> k where
  property :: k -> Lens' s (Maybe p)
  properties :: s -> V.Vector (Maybe p)

  find :: s -> p -> Maybe k
  findKeys :: s -> (p -> Bool) -> [k]
