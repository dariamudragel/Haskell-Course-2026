module UiLayoutLang.Parser(parseWindow,parseLayout)where

import UiLayoutLang.Types
import Data.Void
import Data.Char(isAlphaNum)
import Control.Monad(void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser=Parsec Void String

sc::Parser()
sc=L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme::Parser a->Parser a
lexeme=L.lexeme sc
symbol::String->Parser String
symbol=L.symbol sc
word::String->Parser String
word s=lexeme(try(string s<*notFollowedBy alphaNumChar))

parseWindow::String->Either String Window
parseWindow s=case runParser(pWindow<*eof)"input"s of
 Left e->Left(errorBundlePretty e)
 Right x->Right x

parseLayout::String->Either String Layout
parseLayout s=case runParser(pLayout<*eof)"input"s of
 Left e->Left(errorBundlePretty e)
 Right x->Right x

pWindow::Parser Window
pWindow=do
 sc
 void(word "window")
 name<-lexeme(char '"'>>manyTill L.charLiteral(char '"'))
 w<-lexeme L.decimal
 void(symbol "x")
 h<-lexeme L.decimal
 lay<-braces pLayout
 pure(Window name w h lay)

pLayout::Parser Layout
pLayout=try pContainer<|>pBox

pContainer::Parser Layout
pContainer=do
 d<-(word "row">>pure Row)<|>(word "col">>pure Col)
 kids<-braces(many pLayout)
 pure(Box(defaultProps{pDir=d})kids)

pBox::Parser Layout
pBox=do
 void(word "box")
 (ps,kids)<-braces propsAndChildren
 pure(Box ps kids)

propsAndChildren::Parser(Props,[Layout])
propsAndChildren=go defaultProps[] where
 go ps kids=(do void(symbol "width");void(symbol ":");s<-pSize;semi;go(ps{pWidth=s})kids)
  <|>(do void(symbol "height");void(symbol ":");s<-pSize;semi;go(ps{pHeight=s})kids)
  <|>(do void(symbol "dir");void(symbol ":");d<-parseDir;semi;go(ps{pDir=d})kids)
  <|>(do void(symbol "color");void(symbol ":");c<-pIdent;semi;go(ps{pColor=Just c})kids)
  <|>(do k<-pLayout;go ps(kids++[k]))
  <|>pure(ps,kids)

pSize::Parser Size
pSize=try(do n<-lexeme L.decimal;void(symbol "%");pure(Pct(fromIntegral(n::Int)/100)))
 <|>try(do n<-lexeme L.float;void(symbol "%");pure(Pct(n/100)))
 <|>try(do n<-lexeme L.decimal;void(symbol "px");pure(Px n))
 <|>(word "auto">>pure Auto)
 <|>(Px <$> lexeme L.decimal)

parseDir::Parser Direction
parseDir=(Row<$symbol "row")<|>(Col<$symbol "col")

pIdent::Parser String
pIdent=lexeme(some(satisfy(\c->isAlphaNum c||c=='_'||c=='-')))

braces::Parser a->Parser a
braces=between(symbol "{")(symbol "}")

semi::Parser()
semi=void(optional(symbol ";"))
