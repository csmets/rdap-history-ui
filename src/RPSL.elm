module RPSL exposing (RpslAttr (..), RPSL (..), fromString)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import String exposing (cons)

type RpslAttr = RpslAttr String String
type RPSL = RPSL (List RpslAttr)

fromString : String -> Result (List String) RPSL
fromString s = fst <| parse parseRpsl s

parseRpsl : Parser RPSL
parseRpsl = map RPSL <| sepBy (string "\n") attribute

attribute : Parser RpslAttr
attribute = RpslAttr <$> attrName <*> attrValue

attrName : Parser String
attrName = cons <$> letter <*> while ((/=) ':') <* (char ':' <* hSpace)

letter : Parser Char
letter = oneOf (String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

hSpace : Parser ()
hSpace = while (\c -> c == ' ' || c == '\t') *> succeed ()

attrValue : Parser String
attrValue = while ((/=) '\n') -- TODO continuation
