module Util.Optics
  ( oask
  , getv
  ) where

import Optics (Is, A_Getter, view, Optic')
import Control.Monad.Reader
import Control.Monad.State

oask :: (MonadReader s m, Is k A_Getter) => Optic' k is s a -> m a
oask = asks . view

getv :: (MonadState s m, Is k A_Getter) => Optic' k is s a -> m a
getv = gets . view
