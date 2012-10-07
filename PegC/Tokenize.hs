{- Copyright 2012 Dustin DeWeese
   This file is part of pegc.

    pegc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pegc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pegc.  If not, see <http://www.gnu.org/licenses/>.
-}

module PegC.Tokenize where

import PegC.Value

import Control.Applicative
import Debug.Trace
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Monad.State

lexer = P.makeTokenParser haskellDef

integer = P.integer lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
natural = P.natural lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer

word :: Parser Value
word = W <$> ((:) <$> (lower <|> char ':') <*> many (alphaNum <|> oneOf "?_'#"))

atom :: Parser Value
atom = A <$> (((:) <$> upper <*> many (alphaNum <|> oneOf "?_'#")) <|>
              ((:[]) <$>  oneOf "[_"))

var :: Parser Value
var = V <$> (char '?' *> many1 (alphaNum <|> char '_'))

svar :: Parser Value
svar = S <$> (char '@' *> many1 (alphaNum <|> char '_'))

symbol :: Parser Value
symbol = W <$> (many1 (oneOf "!@#$%^&*()-+=<>.~/?\\|") <|>
                fmap (:[]) (oneOf "]{};"))

quote :: Parser Value
quote = L . (:[]) <$> (char '`' *> value)

number :: Parser Value
number = do m <- optionMaybe (char '-')
            let f = maybe (either I F)
                          (const $ either (I . negate) (F . negate)) m
            f <$> naturalOrFloat

value :: Parser Value
value = try number        <|>
        try var           <|>
        try svar          <|>
        try symbol        <|>
        word              <|>
        atom              <|>
        C <$> charLiteral <|>
        L . map C <$> stringLiteral <|>
        quote

comment = string "--" >> many (noneOf "\n")

stackExpr :: Parser [Value]
stackExpr = concatMap f <$> (whiteSpace >> value `sepEndBy` whiteSpace <* optional comment)
  where f (W "{") = [A "[", A "["]
        f (W "}") = [W "]", W "]"]
        f (W ";") = [A "[", W "]"]
        f x = [x]

tokenize = parse stackExpr ""

-------------------- Debug --------------------

probe s x = trace (s ++ show x) x

