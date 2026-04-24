:set -XOverloadedStrings
:set -XTypeApplications
:set -XTemplateHaskell
:set -XEmptyCase
:set -XDataKinds

import qualified Data.Text as T
import           DataFrame.Functions (declareColumns, declareColumnsWithPrefix)
import           Data.Text (Text)
import           DataFrame.Operators
import           Data.Int
import           Data.Time

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Display.Web.Plot as Plt
import qualified DataFrame.Typed as DT
import           DataFrame.Monad

import GHC.Conc

n <- getNumProcessors
setNumCapabilities $ max 1 (min 4 (n-1))

default (Int, T.Text, Double)
