module Rdap exposing (objectClass, handle, render)

-- The renderers in this module are quite specific to APNIC's use profile of RDAP.
-- Pull requests making the rendering less rigidly fixed to one profile are very welcome.

import Html             exposing (..)
-- import Html.Attributes  exposing (class, value, id)
import Json.Decode      exposing (decodeValue, maybe, field, string, map2, list, Value, Decoder)
import Maybe            exposing (Maybe)
import Result

import Model exposing (..)

extract : Value -> (a -> Maybe b) -> Decoder a -> Maybe b
extract v f d = Result.withDefault Nothing
        <| decodeValue (Json.Decode.map (Maybe.andThen f) (maybe d)) v

objectClass : Value -> Maybe ObjectClass
objectClass v = extract v ot (field "objectClassName" string)

handle : Value -> Maybe String
handle v = extract v Just (field "handle" string)

ot : String -> Maybe ObjectClass
ot s = case s of
    "ip network" -> Just InetNum
    "autnum"     -> Just AutNum
    "entity"     -> Just Entity
    "domain"     -> Just Domain
    _            -> Nothing

type alias Remark =
    { title : String
    , description : List String
    }

remarks : Decoder (List Remark)
remarks = list (map2 Remark (field "title" string) (field "description" (list string)))

remark : Remark -> (String, Html a)
remark r = (r.title, pre [] [text <| String.join "\n" r.description])

render : Version -> List (Html a)
render v = case objectClass v.object of
    Just InetNum -> inetnum v.object
    Just AutNum  -> [ text "ASN NYI" ]
    Just Entity  -> [ text "Entity NYI" ]
    Just Domain  -> [ text "Domain NYI" ]
    Nothing      -> [ text "Unrecognised object class" ]

-- This renderer assumes that the object handle contains the network address block
inetnum : Value -> List (Html a)
inetnum v = [ table [] <| List.concatMap (\i -> i v)
    [ labelled "network name"   (field "name"    (Json.Decode.map text string))
    , labelled "network"        (field "handle"  (Json.Decode.map text string))
    , labelled "country"        (field "country" (Json.Decode.map text string))
    , labelled "type"           (field "type"    (Json.Decode.map text string))
    , tabulated                 (field "remarks" (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices" (Json.Decode.map (List.map remark) remarks))
    -- , labelled "Remarks"        (field "remarks" (Json.Decode.map (\s -> text "remarks nyi") remarks))
    ] ]

tabulated : Decoder (List (String, Html a)) -> Value -> List (Html a)
tabulated d v = Result.withDefault []
    <| Result.map (List.map (\(t, v) -> tr [] [ td [] [text t], td [] [v] ])) (decodeValue d v)

labelled : String -> Decoder (Html a) -> Value -> List (Html a)
labelled k d v = Result.withDefault []
    <| Result.map (\h -> [ tr [ ] [ td [ ] [ text k ], td [ ] [ h ] ] ]) (decodeValue d v)
