module IHaskell.Display.Dataframe where

import           IHaskell.Display

import qualified DataFrame as D
import DataFrame (DataFrame)

import qualified Data.Text as T

instance IHaskellDisplay DataFrame where
  display val = return $ Display [stringDisplay, markdownDisplay]
    where
      str = T.unpack (D.toMarkdownTable val)
      stringDisplay = plain str
      markdownDisplay = markdown str
