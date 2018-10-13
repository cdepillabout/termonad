{-# LANGUAGE NoImplicitPrelude #-}

module Termonad.Prelude
  ( module X
  , whenJust
  ) where

import ClassyPrelude as X
import Data.Proxy as X

whenJust :: Monoid m => Maybe a -> (a -> m) -> m
whenJust = flip foldMap
