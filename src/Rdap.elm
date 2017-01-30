module Rdap exposing (objectClass, handle, render)

-- The renderers in this module are quite specific to APNIC's use profile of RDAP.
-- Pull requests making the rendering less rigidly fixed to one profile are very welcome.

import Dict             exposing (Dict)
import Html             exposing (..)
import Html.Attributes  exposing (class, colspan)
import Json.Decode      exposing (..)
import Json.Encode
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

type alias VCardEntry =
    { name : String
    , parameters : Dict String Value
    , kind : String
    , content : Value
    }

decodeVcard : Decoder VCardEntry
decodeVcard = Json.Decode.map4 VCardEntry (index 0 string) (index 1 (dict value)) (index 2 string) (index 3 value)

remarks : Decoder (List Remark)
remarks = list (map2 Remark (field "title" string) (field "description" (list string)))

remark : Remark -> (String, Html a)
remark r = (r.title, pre [] [text <| String.join "\n" r.description])

render : Version -> List (Html a)
render v = case objectClass v.object of
    Just InetNum -> [ table [] <| inetnum v.object ]
    Just AutNum  -> [ text "ASN NYI" ]
    Just Entity  -> [ table [] <| entity  v.object ]
    Just Domain  -> [ text "Domain NYI" ]
    Nothing      -> [ text "Unrecognised object class" ]

-- This renderer assumes that the object handle contains the network address block
inetnum : Value -> List (Html a)
inetnum v = List.concatMap (\i -> i v)
    [ labelled "network name"   (field "name"     (Json.Decode.map text string))
    , labelled "network"        (field "handle"   (Json.Decode.map text string))
    , labelled "country"        (field "country"  (Json.Decode.map text string))
    , labelled "type"           (field "type"     (Json.Decode.map text string))
    , tabulated                 (field "remarks"  (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices"  (Json.Decode.map (List.map remark) remarks))
    , recursively entity        (field "entities" (list value))
    ]

spacer : Html a
spacer = tr [ class "spacer" ] [ td [ colspan 2 ] [ hr [] [] ] ]

recursively : (Value -> List (Html a)) -> Decoder (List Value) -> Value -> List (Html a)
recursively f d v = Result.withDefault []
    <| Result.map (List.concatMap (\x -> spacer :: f x)) (decodeValue d v)

entity : Value -> List (Html a)
entity v = List.concatMap (\i -> i v)
    [ labelled "handle"         (field "handle"   (Json.Decode.map text string))
    , labelled "country"        (field "country"  (Json.Decode.map text string))
    , vcard                     (field "vcardArray" <| index 1 <| list decodeVcard)
    , labelled "roles"          (field "roles"     (Json.Decode.map text string))
    , tabulated                 (field "remarks"  (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices"  (Json.Decode.map (List.map remark) remarks))
    , recursively entity        (field "entities" (list value))
    ]

run : (a -> List (Html b)) -> Decoder a -> Value -> List (Html b)
run f d v = Result.withDefault [] <| Result.map f (decodeValue d v)

row : Html a -> List (Html a) -> Html a
row l r = tr [] [ td [] [ l ], td [] r ]

vcard : Decoder (List VCardEntry) -> Value -> List (Html a)
vcard d v = Result.withDefault (Debug.log "failed vcard decode" []) <| Result.map (List.concatMap vcardEntry << List.drop 1) (decodeValue d v)

-- decode a structured jCard value to a string, given a separator and a component decoder
structured : String -> Decoder String -> Decoder String
structured sep part = Json.Decode.map (String.join sep) (list part)

-- like `text`, but will produce interspersed text and <br>
newlined : String -> List (Html a)
newlined s = String.split "\n" s
        |> List.map text
        |> List.intersperse (br [] [])

adr : VCardEntry -> List (Html a)
adr v = Dict.get "label" v.parameters
        |> Result.fromMaybe "unused error description"
        |> Result.andThen (decodeValue string)
        |> Result.map Ok
        |> Result.withDefault (decodeValue (structured ";" (oneOf [string, structured "," string])) v.content)
        |> Result.map (\a -> [ row (text "address") (newlined a) ])
        |> Result.withDefault []

simple : String -> VCardEntry -> List (Html a)
simple l v = run (\n -> [ row (text l) [text n] ]) string v.content

-- The "tel" vCard spec is a complex piece of work, this function will only recognise TYPE=fax, and only if it's the
-- only type parameter: ["work","fax"] will be considered a voice number, for instance.
tel : VCardEntry -> List (Html a)
tel v = Dict.get "type" v.parameters
        |> Result.fromMaybe "unused error description"
        |> Result.andThen (decodeValue string)
        |> Result.withDefault "no type parameter"
        |> \t -> (if t /= "fax" then "voice" else "fax")
        |> \t -> run (\n -> [ row (text t) [text n] ]) string v.content

-- This is only a partial decode of jCard attributes
vcardEntry : VCardEntry -> List (Html a)
vcardEntry v = case v.name of
    "fn"    -> simple "name" v
    "kind"  -> simple "kind" v
    "adr"   -> adr v
    "tel"   -> tel v
    "email" -> simple "email" v
    -- the default is to show raw JSON data; could do better
    _       -> run (\raw -> [ row (text v.name) [ text (Json.Encode.encode 0 raw) ] ]) value v.content

tabulated : Decoder (List (String, Html a)) -> Value -> List (Html a)
tabulated = run <| List.map (\(t, v) -> row (text t) [ v ])

labelled : String -> Decoder (Html a) -> Value -> List (Html a)
labelled k = run <| \h -> [ row (text k) [ h ] ]
