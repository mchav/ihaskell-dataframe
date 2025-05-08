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
    -- Each table should have its own unique IDs
    -- suffixed by a random number so filtering doesn't change
    -- all table values.
    r <- T.pack . show <$> randomRIO @Int (0, 10000)
    return $ Display [stringDisplay r, htmlDisplay r]
      where
        columns = fst (D.dimensions val)
        str r' = table r' (D.columnNames val) (map (D.getRowAsText val) [0..(columns - 1)])
        stringDisplay r' = plain (str r')
        htmlDisplay r' = html' (Just (style r')) (str r')


style :: T.Text -> T.Text
style r = "\n\
  \#dfSearchInput-" <> r <> " {\n\
  \  box-sizing: border-box;\n\
  \  background-image: url('https://img.icons8.com/ios-glyphs/30/search--v1.png');\n\
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
  \  padding: 1em;\n\
  \}\n\
  \\n\
  \#dataframeTable-" <> r <> " tr {\n\
  \  box-sizing: border-box;\n\
  \  border-bottom: 0.075em solid #ddd;\n\
  \}\n\
  \\n\
  \#dataframeTable-" <> r <> " tr.header, #dataframeTable-" <> r <> " tr:hover {\n\
  \  background-color: #f1f1f1;\n\
  \}\n\
  \td.df-cell-" <> r <> " { {\n\
  \  box-sizing: border-box;\n\
  \  resize: both;\n\
  \  overflow: auto;\n\
  \  width: 10em;\n\
  \  height: 10em;\n\
  \  margin: 0em;\n\
  \  padding: 0em;\n\
  \  border: 0.075em solid black;\n\
  \  display:block;\n\
  \}\n\
  \td.df-cell-" <> r <> " > .df-cell__content-" <> r <> " { {\n\
  \  box-sizing: border-box;\n\
  \  border: 0;\n\
  \  width: auto;\n\
  \  height: auto;\n\
  \  min-height: 1.5em;\n\
  \  min-width: 1.5em;\n\
  \}\n"

mkOptions :: [T.Text] -> T.Text
mkOptions = mconcat . map (\h -> "<option value=\"" <> h <> "\">" <> h <> "</option>\n")

mkHeader :: [T.Text] -> T.Text
mkHeader = mconcat . map (\h -> "<th>" <> h <> "</th>\n")

mkCells :: [[T.Text]] -> T.Text
mkCells = mconcat . map (\b -> "<tr> \n" <> textRow b <> "</tr>\n")
  where textRow = mconcat . map (\b' -> "<td class=\"df-cell\"><div class=\"df-cell__content\"> " <> b' <> "</div></td>\n")

table :: T.Text -> [T.Text] -> [[T.Text]] -> String
table r header body= T.unpack $ "\
  \ <input type=\"text\" id=\"dfSearchInput-" <> r <> "\" onkeyup=\"filterDataframe()\" placeholder=\"Search by field..\" title=\"Type in a value\"> \n \
  \ <label for=\"filters-" <> r <> "\">Choose a field to filter by:</label> \n \
  \ <select id=\"filters-" <> r <> "\" name=\"filters\">\n" <> mkOptions header <>
  "\
  \ </select> \n \
  \ <table id=\"dataframeTable-" <> r <> "\"> \n\
  \    <tr class=\"header\">" <> mkHeader header <>
  "    </tr>\n" <> mkCells body <>
  "\
  \  </table>\n\
  \  <script>\n\
  \ (() => {\n\
  \ const table     = document.getElementById(\"dataframeTable-" <> r <> "\");\n\
  \ const rows      = Array.from(table.querySelectorAll('tr)).slice(1);\n\
  \ const headers   = Array.from(table.querySelectorAll('th'));\n\
  \ const input     = document.getElementById(\"searchInput-" <> r <> "\");\n\
  \ const filters   = document.getElementById(\"filters-" <> r <> "\");\n\
  \ let columnIdx = 0;\n\
  \ filters.addEventListener('change', () => {\n\
  \   columnIdx = headers.findIndex(th => th.textContent === filters.value);\n\
  \   filter();\n\
  \ });\n\
  \ input.addEventListener('input', debounce(filter, 150));\n\
  \ function filter () {\n\
  \   const term = input.value.trim().toUpperCase();\n\
  \   if (!term) {\n\
  \     rows.forEach(r => r.classList.remove('hiddenRow'));\n\
  \     return;\n\
  \   }\n\
  \   rows.forEach(row => {\n\
  \     const cellText = row.cells[columnIdx].textContent.toUpperCase();\n\
  \     row.classList.toggle('hiddenRow', !cellText.includes(term));\n\
  \   });\n\
  \ }\n\
  \ function debounce (fn, delay = 200) {\n\
  \   let t;\n\
  \   return (...args) => {\n\
  \     clearTimeout(t);\n\
  \     t = setTimeout(() => fn.apply(this, args), delay);\n\
  \   };\n\
  \ }\n\
  \ })();\n\
  \  </script>\n"
