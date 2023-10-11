
-- | This is a basic prelude-like module that adds a bunch of common datatypes
-- and functions to the 'Prelude'.

module Termonad.Prelude
  ( module X
  , hPutStrLn
  , whenJust
  , stderr
  , tshow
  ) where

import Control.Concurrent.MVar as X (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception as X (IOException, try)
import Control.Lens as X ((&))
import Control.Monad as X (join, unless, void, when, (<=<))
import Control.Monad.IO.Class as X
import Control.Monad.Trans.Class as X (lift)
import Control.Monad.Trans.Maybe as X (MaybeT(MaybeT), runMaybeT)
import Data.ByteString as X (ByteString)
import Data.Function as X (on)
import Data.Functor as X (($>))
import Data.Int as X (Int32)
import Data.Maybe as X (catMaybes, fromMaybe)
import Data.Proxy as X
import Data.String as X (IsString)
import Data.Text as X (Text)
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import Data.Word as X (Word8, Word32)
import GHC.Generics as X (Generic)
import Prelude as X
import System.IO (Handle, stderr)

whenJust :: Monoid m => Maybe a -> (a -> m) -> m
whenJust = flip foldMap

hPutStrLn :: MonadIO m => Handle -> Text -> m ()
hPutStrLn hndl = liftIO . TextIO.hPutStrLn hndl

tshow :: Show a => a -> Text
tshow = pack . show
