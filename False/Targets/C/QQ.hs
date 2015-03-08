-- |
-- Module      : False.Targets.C.QQ
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- QuasiQuoter definition must be seperate from the use.

module False.Targets.C.QQ (include) where

import Language.Haskell.TH.Quote (QuasiQuoter,quoteFile)
import Text.RawString.QQ (r)


include :: QuasiQuoter
include = quoteFile r
