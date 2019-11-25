
module Main where

import Termonad (defaultMain)
import Termonad.Config (tmConfigFromPreferencesFile)

main :: IO ()
main = defaultMain =<< tmConfigFromPreferencesFile
