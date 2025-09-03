module IHaskell.Display.Dataframe where

import           IHaskell.Display

import qualified DataFrame as D
import qualified DataFrame.Display.Web.Plot as Plot
import DataFrame (DataFrame)

import qualified Data.Text as T

instance IHaskellDisplay DataFrame where
  display val = return $ Display [stringDisplay, markdownDisplay]
    where
      str = T.unpack (D.toMarkdownTable val)
      stringDisplay = plain str
      markdownDisplay = markdown str

instance IHaskellDisplay Plot.HtmlPlot where
  display (HtmlPlot val) = return $ Display [stringDisplay, markdownDisplay]
    where
      stringDisplay = plain (T.unpack val)
      htmlDisplay = html (T.unpack val)
