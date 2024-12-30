module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" content
        )
    )

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

pre_ :: String -> Structure
pre_ = Structure . el "pre" . escape

ol_ :: [Structure] -> Structure
ol_ =
  let createListItem (Structure li) = el "li" li
  in Structure . el "ol" . concatMap createListItem

ul_ :: [Structure] -> Structure
ul_ =
  let createListItem (Structure li) = el "li" li
  in Structure . el "ul" . concatMap createListItem

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure content) = content

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar
