{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IHaskell.Display.Dataframe where

import           IHaskell.Display

import qualified DataFrame as D
import DataFrame (DataFrame)

import qualified Data.Text as T
import System.Random (randomRIO)
  
instance IHaskellDisplay DataFrame where
  display :: DataFrame -> IO Display
  display val = do
    r <- T.pack . show <$> randomRIO @Int (0, 10000)
    return $ Display [stringDisplay r, htmlDisplay r]
      where
        columns = fst (D.dimensions val)
        str r' = table (T.unpack r') (D.columnNames val) (map (D.getRowAsText val) [0..(columns - 1)])
        stringDisplay r' = plain (str r')
        htmlDisplay r' = html' (Just (style r')) (str r')


style :: T.Text -> T.Text
style r = "\n\
  \#dfSearchInput-" <> r <> " {\n\
  \  box-sizing: border-box;\n\
  \  background-image: url('/css/searchicon.png');\n\
  \  background-position: 0.75em 0.75em;\n\
  \  background-repeat: no-repeat;\n\
  \  width: 100%;\n\
  \  font-size: 1em;\n\
  \  padding: 0.8em 1.25em 0.8em 3em;\n\
  \  border: 0.075em solid #ddd;\n\
  \  margin-bottom: 0.8em;\n\
  \}\n\
  \\n\
  \#dataframeTable-" <> r <> " {\n\
  \  box-sizing: border-box;\n\
  \  border-collapse: collapse;\n\
  \  width: 100%;\n\
  \  border: 0.075em solid #ddd;\n\
  \  font-size: 1.25em;\n\
  \}\n\
  \\n\
  \#dataframeTable-" <> r <> " th, #dataframeTable-" <> r <> " td {\n\
  \  box-sizing: border-box;\n\
  \  text-align: left;\n\
  \  padding: 12px;\n\
  \}\n\
  \\n\
  \#dataframeTable-" <> r <> " tr {\n\
  \  box-sizing: border-box;\n\
  \  border-bottom: 1px solid #ddd;\n\
  \}\n\
  \\n\
  \#dataframeTable-" <> r <> " tr.header, #dataframeTable-" <> r <> " tr:hover {\n\
  \  background-color: #f1f1f1;\n\
  \}\n\
  \td.df-cell-" <> r <> " { {\n\
  \  box-sizing: border-box;\n\
  \  resize: both;\n\
  \  overflow: auto;\n\
  \  width: 120px;\n\
  \  height: 120px;\n\
  \  margin: 0px;\n\
  \  padding: 0px;\n\
  \  border: 1px solid black;\n\
  \  display:block;\n\
  \}\n\
  \td.df-cell-" <> r <> " > .df-cell__content-" <> r <> " { {\n\
  \  box-sizing: border-box;\n\
  \  border: 0;\n\
  \  width: auto;\n\
  \  height: auto;\n\
  \  min-height: 20px;\n\
  \  min-width: 20px;\n\
  \}\n"

mkOptions :: [T.Text] -> String
mkOptions = mconcat . map ((\h -> "<option value=\"" ++ h ++ "\">" ++ h ++ "</option>\n") . T.unpack)

mkHeader :: [T.Text] -> String
mkHeader = mconcat . map ((\h -> "<th>" ++ h ++ "</th>\n"). T.unpack)

mkCells :: [[T.Text]] -> String
mkCells = mconcat . map (\b -> "<tr> \n" ++ textRow b ++ "</tr>\n")
  where textRow = mconcat . map ((\b' -> "<td class=\"df-cell\"><div class=\"df-cell__content\"> " ++ b' ++ "</div></td>\n") . T.unpack)

table :: String -> [T.Text] -> [[T.Text]] -> String
table r header body= "\
  \ <input type=\"text\" id=\"dfSearchInput-" <> r <> "\" onkeyup=\"filterDataframe()\" placeholder=\"Search by field..\" title=\"Type in a value\"> \n \
  \ <label for=\"filters-" <> r <> "\">Choose a field to filter by:</label> \n \
  \ <select id=\"filters-" <> r <> "\" name=\"filters\">\n" ++ mkOptions header ++
  "\
  \ </select> \n \
  \ <table id=\"dataframeTable-" <> r <> "\"> \n\
  \    <tr class=\"header\">" ++ mkHeader header ++
  "    </tr>\n" ++ mkCells body ++
  "\
  \  </table>\n\
  \  <script>\n\
  \  function filterDataframe() {\n\
  \    var input, filter, table, tr, td, i, txtValue;\n\
  \    input = document.getElementById(\"dfSearchInput-" <> r <> "\");\n\
  \    filter = input.value.toUpperCase();\n\
  \    table = document.getElementById(\"dataframeTable-" <> r <> "\");\n\
  \    tr = table.getElementsByTagName(\"tr\");\n\   
  \    var e = document.getElementById(\"filters-" <> r <> "\");\n\
  \    var value = e.value\n\
  \    var index;\n\
  \    for (i = 0; i < tr[0].getElementsByTagName(\"th\").length; i++) {\n\
  \      if (tr[0].getElementsByTagName(\"th\")[i].innerText == value) {\n\
  \        index = i;\n\
  \      }\n\
  \    }\n\  
  \    for (i = 0; i < tr.length; i++) {\n\
  \      td = tr[i].getElementsByTagName(\"td\")[index];\n\
  \      if (td) {\n\
  \        txtValue = td.textContent || td.innerText;\n\
  \        if (txtValue.toUpperCase().indexOf(filter) > -1) {\n\
  \          tr[i].style.display = \"\";\n\
  \        } else {\n\
  \          tr[i].style.display = \"none\";\n\
  \        }\n\
  \      }\n\
  \    }\n\
  \  }\n\
  \  </script>\n"
