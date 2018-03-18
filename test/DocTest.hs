
module Main (main) where

import Prelude

import Data.Monoid ((<>))
import Test.DocTest (doctest)

main :: IO ()
main = doctest $ ["src/"] <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [
    --   "-XConstraintKinds"
    -- , "-XDataKinds"
    -- , "-XDeriveDataTypeable"
    -- , "-XDeriveGeneric"
    -- , "-XEmptyDataDecls"
    -- , "-XFlexibleContexts"
    -- , "-XFlexibleInstances"
    -- , "-XGADTs"
    -- , "-XGeneralizedNewtypeDeriving"
    -- , "-XInstanceSigs"
    -- , "-XMultiParamTypeClasses"
    -- , "-XNoImplicitPrelude"
    -- , "-XOverloadedStrings"
    -- , "-XPolyKinds"
    -- , "-XRankNTypes"
    -- , "-XRecordWildCards"
    -- , "-XScopedTypeVariables"
    -- , "-XStandaloneDeriving"
    -- , "-XTupleSections"
    -- , "-XTypeFamilies"
    -- , "-XTypeOperators"
    ]
