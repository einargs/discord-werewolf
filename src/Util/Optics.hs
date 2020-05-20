module Util.Optics
  ( oask
  ) where

import Optics (Is, A_Getter, view, Optic')
import Control.Monad.Reader

oask :: (MonadReader s m, Is k A_Getter) => Optic' k is s a -> m a
oask = asks . view
