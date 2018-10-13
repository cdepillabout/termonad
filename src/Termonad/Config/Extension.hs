
module Termonad.Config.Extension
  ( ConfigExtension(..)
  , SomeConfigExtension(..)
  , ConfigHooks(..)
  , Message
  , Both(..)
  , (<+>)
  , fromMessage
  , sendMessage
  ) where

import Termonad.Prelude
import Termonad.Lenses
import Termonad.Types
  ( ConfigExtension(..)
  , ConfigHooks(..)
  , SomeConfigExtension(..)
  , Message
  , TMState
  , TMConfig(extension)
  )

import Control.Lens ((^.), (.~), (&))
import Data.Typeable (cast)

-- | Attempt to recover a message of known type given a @Message m => m@ value.
--   An alias of 'cast'.
fromMessage :: (Typeable a, Typeable b) => a -> Maybe b
fromMessage = cast

-- | A more convenient interface to 'message' that also ensures messages are
--   interpreted atomically with respect to the supplied @TMState@ MVar (so long
--   as it has no other producers).
sendMessage :: Message m => TMState -> m -> IO ()
sendMessage mvarTMState m = do
  tmState <- takeMVar mvarTMState
  let confExtension = tmState ^. lensTMStateConfig . lensExtension
  messageMVar <- newMVar tmState
  confExtension' <- message messageMVar m confExtension
  let tmState' = tmState & lensTMStateConfig . lensExtension .~ confExtension'
  putMVar mvarTMState tmState'

-- | A config extension combinator.
--   Is used internally by '<+>' and should not need to be used directly.
data Both g1 g2 = Both !g1 !g2

instance (ConfigExtension g1, ConfigExtension g2) => ConfigExtension (Both g1 g2) where

  hooks :: Both g1 g2 -> ConfigHooks
  hooks (Both g1 g2) = hooks g1 <> hooks g2

  message :: Message m => TMState -> m -> Both g1 g2 -> IO (Both g1 g2)
  message mvarTMState m (Both g1 g2) =
    Both <$> message mvarTMState m g1 <*> message mvarTMState m g2

-- | Incorporate a config extension into a @TMConfig@.
(<+>) :: ConfigExtension g => TMConfig -> g -> TMConfig
tmConf <+> g = tmConf
  { extension = case extension tmConf of
      SomeConfigExtension sg -> SomeConfigExtension (Both sg g)
  }

infixl 6 <+>
