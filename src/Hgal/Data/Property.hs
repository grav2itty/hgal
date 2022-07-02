module Hgal.Data.Property where

import Control.Lens
import qualified Data.Vector as V

class Property s k p | s k -> p, s p -> k where
  property :: k -> Lens' s (Maybe p)
  properties :: s -> V.Vector (Maybe p)
