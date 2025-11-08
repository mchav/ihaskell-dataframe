:set -XOverloadedStrings
:set -XTypeApplications
:set -XTemplateHaskell

import qualified Data.Text as T
import           DataFrame.Functions (declareColumns)
import           Data.Text (Text)
import           DataFrame ((|>))
import           Data.Int
import           Data.Time

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import           DataFrame.Functions ((.==), (.<=), (.>=), (.<), (.>), (.&&), (.||))

import GHC.Conc

n <- getNumProcessors
setNumCapabilities $ max 1 (min 4 (n-1))

default (Int, T.Text, Double)
