{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Other.QQHtml where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Parsec
import           Text.Parsec.String


data Node = Tag String [Node]
          | Text String
          deriving Show

-- first defien a normal parser
textNode :: Parser Node
textNode = Text <$> many1 (satisfy (/= '<'))

tagNode :: Parser Node
tagNode = do
  tagName <- char '<' *> many1 letter <* char '>'
  children <- many $ try tagNode <|> textNode
  string "</" *> string tagName *> char '>'
  return $ Tag tagName children

instance Lift Node where
  lift (Text t)            = [| Text t |]
  lift (Tag name children) = [| Tag name children |]

htmlExpr :: String -> Q Exp
htmlExpr str = do
  filename <- loc_filename <$> location
  case parse tagNode filename str of
    Left _    -> lift "error"
    Right tag -> [| tag |]

-- quasi quotes for patterns
htmlPat :: String -> Q Pat
htmlPat "<_>"   = [p| Tag _ children |]
htmlPat "<strong>"   = [p| Tag "strong" children |]
htmlPat "#text" = [p| Text text |]
htmlPat ('<':rest) = return $
  ConP (mkName "HTML.tag")
    [ LitP (StringL (init rest))
    , VarP (mkName "children")]

html :: QuasiQuoter
html = QuasiQuoter
  htmlExpr
  htmlPat
  undefined
  undefined
