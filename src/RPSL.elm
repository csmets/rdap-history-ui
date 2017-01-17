module RPSL exposing (RpslAttr(..), RPSL(..), fromString, primaryKey)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Result exposing (andThen, fromMaybe)
import String exposing (cons, append, concat)


type RpslAttr
    = RpslAttr String String


type RPSL
    = RPSL (List RpslAttr)


primaryKey : String -> Result (List String) RpslAttr
primaryKey s =
    let
        rpsl =
            fromString s

        getFirstAttr (RPSL attrs) =
            fromMaybe [ "No attributes found" ] <|
                List.head attrs

        first =
            Result.andThen rpsl getFirstAttr

        isNicHdl (RpslAttr k v) =
            k == "nic-hdl"

        getNicHdl (RPSL attrs) =
            fromMaybe [ "Missing nic-hdl attribute" ] <|
                List.head <|
                    List.filter isNicHdl attrs

        isEntity (RpslAttr k v) =
            k == "person" || k == "role"

        entityToHandle a =
            if isEntity a then
                Result.andThen rpsl getNicHdl
            else
                Ok a

        pkey =
            Result.andThen first entityToHandle
    in
        pkey


fromString : String -> Result (List String) RPSL
fromString s =
    Tuple.first <| parse parseRpsl s


parseRpsl : Parser RPSL
parseRpsl =
    map RPSL <| sepBy (string "\n") attribute


attribute : Parser RpslAttr
attribute =
    RpslAttr <$> attrName <*> attrValue


attrName : Parser String
attrName =
    cons <$> letter <*> while ((/=) ':') <* (char ':' <* hSpace)


letter : Parser Char
letter =
    oneOf (String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")


hSpace : Parser ()
hSpace =
    while (\c -> c == ' ' || c == '\t') *> succeed ()



-- nb: does not fuss with comments


attrValue : Parser String
attrValue =
    append <$> while ((/=) '\n') <*> (concat <$> many continuation)


continuation : Parser String
continuation =
    char '\n' *> oneOf [ ' ', '\t', '+' ] *> while ((/=) '\n')
