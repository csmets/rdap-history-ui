module Decode exposing (history)

import Date exposing (Date)
import Json.Decode exposing (..)
import List
import Maybe
import Tuple

import Model exposing (..)
import Rdap exposing (..)

-- overly specific groupBy doing too many things
groupBy : (a -> k) -> (k -> comparable) -> (a -> b) -> List a -> List (k, List b)
groupBy kf cf vf l =
    let keys   = List.map kf l
        zipped = List.sortBy (Tuple.first >> cf) <| List.map2 (,) keys l
        m (k, v) l = case l of
            ((k_, v_)::t) -> if k_ == k then (k, vf v::v_)::t else (k, [vf v])::l
            otherwise    -> (k, [vf v])::l
     in List.foldl m [] zipped

history : Decoder (List History)
history = field "records" (list record)
        |> map (List.filterMap addGroupingKey)              -- convert to Maybe ((ObjectClass, Handle), Version)
        |> map (groupBy Tuple.first toComparable (\(_, vs) -> vs))    -- group records by (objectClass, handle)
        |> map (List.map toHistory)         -- covert to (List History)

toHistory : ((ObjectClass, String), List Version) -> History
toHistory ((oc, h), vs) = History (Identifier oc h) vs

toComparable : (ObjectClass, String) -> (String, String)
toComparable (c, h) = (toString c, h)

addGroupingKey : Version -> Maybe ((ObjectClass, String), Version)
addGroupingKey v =
    Maybe.map2 (\oc h -> ((oc, h), v)) (objectClass v.object) (handle v.object)

-- decode one version
record : Decoder Version
record = map3 Version
    (field "applicableFrom" date)
    (maybe (field "applicableUntil" date))
    (field "content" value)

date : Decoder Date
date = string |> andThen (\s -> case Date.fromString s of
    Ok d  -> succeed d
    Err e -> fail e)
