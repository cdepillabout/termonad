module Termonad.Prelude
  ( module X
  , hPutStrLn
  , whenJust
  ) where

import Control.Lens as X ((&))
import ClassyPrelude as X
import Data.Proxy as X
import qualified Data.Text.IO as TextIO

whenJust :: Monoid m => Maybe a -> (a -> m) -> m
whenJust = flip foldMap

hPutStrLn :: MonadIO m => Handle -> Text -> m ()
hPutStrLn hndl = liftIO . TextIO.hPutStrLn hndl
