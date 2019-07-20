{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.Pcre where

import Foreign.C (CUInt)
import qualified Language.C.Inline as C

C.verbatim "#define PCRE2_CODE_UNIT_WIDTH 0"
C.include "<pcre2.h>"

pcre2Multiline :: CUInt
pcre2Multiline = [C.pure| unsigned int { PCRE2_MULTILINE } |]
