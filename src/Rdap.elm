module Rdap exposing (objectClass, handle, render, output, diff)

-- The renderers in this module are quite specific to APNIC's use profile of RDAP.
-- Pull requests making the rendering less rigidly fixed to one profile are very welcome.

import Dict             exposing (Dict)
import Diff             exposing (Change(..))
import Html             exposing (..)
import Html.Attributes  exposing (class, colspan)
import Json.Decode      exposing (..)
import Json.Encode
import Maybe            exposing (Maybe)
import Result

import Model exposing (..)

type alias Remark = { title : String , description : List String }
type alias VCardEntry =
    { name : String
    , parameters : Dict String Value
    , kind : String
    , content : Value
    }

-- A structure to represent an RDAP object interpreted for display
type DisplayMode = Text | Preformatted
type alias DisplayLine a = { a | label : String, value : String, display : DisplayMode }
type alias DisplayObject a = List (DisplayLine a)
type alias DisplayRecord a = { identifier : Identifier, object : DisplayObject a }
type alias RdapDisplay a = List (DisplayRecord a)

-- Annotating a display line with a diff state
type DiffMode = Unchanged | Modified | New
type alias Diff = { diffMode : DiffMode }

extract : Value -> (a -> Maybe b) -> Decoder a -> Maybe b
extract v f d = Result.withDefault Nothing
        <| decodeValue (Json.Decode.map (Maybe.andThen f) (maybe d)) v

objectClass : Value -> Maybe ObjectClass
objectClass v = extract v ot (field "objectClassName" string)

ot : String -> Maybe ObjectClass
ot s = case s of
    "ip network" -> Just InetNum
    "autnum"     -> Just AutNum
    "entity"     -> Just Entity
    "domain"     -> Just Domain
    _            -> Nothing

handle : Value -> Maybe String
handle v = extract v Just (field "handle" string)

identifier : Value -> Maybe Identifier
identifier value = Maybe.map2 Identifier (objectClass value) (handle value)

{- Decoders -}

decodeVcard : Decoder VCardEntry
decodeVcard = Json.Decode.map4 VCardEntry (index 0 string) (index 1 (dict value)) (index 2 string) (index 3 value)

remarks : Decoder (List Remark)
remarks = list (map2 Remark (field "title" string) (field "description" (list string)))

render : Identifier -> Value -> RdapDisplay {}
render i value = case i.objectClass of
    InetNum -> { identifier = i, object = inetnum value } :: entities value
    AutNum  -> entities value
    Entity  -> { identifier = i, object = entity value } :: entities value
    Domain  -> entities value

-- Extract all the entities linked in an RDAP object
entities : Value -> RdapDisplay {}
entities v = field "entities" (list value)
        |> Json.Decode.map (List.concatMap mapObject)
        |> \d -> Result.withDefault [] (decodeValue d v)

mapObject : Value -> RdapDisplay {}
mapObject v = identifier v
        |> Maybe.map (\i -> [{ identifier = i, object = entity v }])
        |> Maybe.withDefault []

    -- like `text`, but will produce interspersed text and <br>
newlined : String -> List (Html a)
newlined s = String.split "\n" s
        |> List.map text
        |> List.intersperse (br [] [])
    
output : RdapDisplay Diff -> Html a
output rdap = table []
        <| List.concat
        <| List.intersperse [spacer]
        <| List.map (.object >> object) rdap

object : DisplayObject Diff -> List (Html a)
object lines = List.map line lines

line : DisplayLine Diff -> Html a
line { label, value, display, diffMode } = case display of
    Text         -> row diffMode (text label) (newlined value)
    Preformatted -> row diffMode (text label) [ pre [] [ text value ] ]

row : DiffMode -> Html a -> List (Html a) -> Html a
row d l r = tr [ diffattr d ] [ td [] [ l ], td [] r ]

diffattr : DiffMode -> Attribute a
diffattr d = case d of
    Unchanged   -> class "diff-unchanged"
    Modified    -> class "diff-modified"
    New         -> class "diff-new"

spacer : Html a
spacer = tr [ class "spacer" ] [ td [ colspan 2 ] [ hr [] [] ] ]

{- Functions to assist rendering into an RdapDisplay -}

display : String -> String -> DisplayLine {}
display l v = { label = l, value = v, display = Text }

run : (a -> List b) -> Decoder a -> Value -> List b
run f d v = Result.withDefault [] <| Result.map f (decodeValue d v)

labelled : String -> Decoder String -> Value -> DisplayObject {}
labelled k d v = (run <| \h -> [ display k h ]) d v

tabulated : Decoder (List (DisplayLine a)) -> Value -> DisplayObject a
tabulated = run identity

remark : Remark -> DisplayLine {}
remark r = { label = r.title, value = String.join "\n" r.description, display = Preformatted }

{- Renderers for the RDAP objects -}

-- This renderer assumes that the object handle contains the network address block
inetnum : Value -> DisplayObject {}
inetnum v = List.concatMap (\i -> i v)
    [ labelled "network name"   (field "name"     string)
    , labelled "network"        (field "handle"   string)
    , labelled "country"        (field "country"  string)
    , labelled "type"           (field "type"     string)
    , tabulated                 (field "remarks"  (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices"  (Json.Decode.map (List.map remark) remarks))
    ]

entity : Value -> DisplayObject {}
entity v = List.concatMap (\i -> i v)
    [ labelled "handle"         (field "handle"   string)
    , labelled "country"        (field "country"  string)
    , vcard                     (field "vcardArray" <| index 1 <| list decodeVcard)
    , labelled "roles"          (field "roles"     string)
    , tabulated                 (field "remarks"  (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices"  (Json.Decode.map (List.map remark) remarks))
    ]

vcard : Decoder (List VCardEntry) -> Value -> DisplayObject {}
vcard d v = Result.withDefault []
        <| Result.map (List.concatMap vcardEntry << List.drop 1) (decodeValue d v)

-- decode a structured jCard value to a string, given a separator and a component decoder
structured : String -> Decoder String -> Decoder String
structured sep part = Json.Decode.map (String.join sep) (list part)

adr : VCardEntry -> DisplayObject {}
adr v = Dict.get "label" v.parameters
        |> Result.fromMaybe "unused error description"
        |> Result.andThen (decodeValue string)
        |> Result.map Ok
        |> Result.withDefault (decodeValue (structured ";" (oneOf [string, structured "," string])) v.content)
        |> Result.map (\a -> [ display "address" a ])
        |> Result.withDefault []

simple : String -> VCardEntry -> DisplayObject {}
simple l v = run (\n -> [ display l n ]) string v.content

-- The "tel" vCard spec is a complex piece of work, this function will only recognise TYPE=fax, and only if it's the
-- only type parameter: ["work","fax"] will be considered a voice number, for instance.
tel : VCardEntry -> DisplayObject {}
tel v = Dict.get "type" v.parameters
        |> Result.fromMaybe "unused error description"
        |> Result.andThen (decodeValue string)
        |> Result.withDefault "no type parameter"
        |> \t -> (if t /= "fax" then "voice" else "fax")
        |> \t -> run (\n -> [ display t n ]) string v.content

-- This is only a partial decode of jCard attributes
vcardEntry : VCardEntry -> DisplayObject {}
vcardEntry v = case v.name of
    "fn"    -> simple "name" v
    "kind"  -> simple "kind" v
    "adr"   -> adr v
    "tel"   -> tel v
    "email" -> simple "email" v
    -- the default is to show raw JSON data; could do better
    _       -> run (\raw -> [ display v.name (Json.Encode.encode 0 raw) ]) value v.content

{- Diffs between two RdapDisplays -}
diff : Maybe (RdapDisplay {}) -> RdapDisplay {} -> RdapDisplay Diff
diff mOrig new = case mOrig of
    Nothing     -> List.map (using Unchanged) new
    Just orig   -> List.map (diffRecord orig) new

-- Return the first element of the list satisfying the predicate, if any
lookup : (a -> Bool) -> List a -> Maybe a
lookup f xs = case xs of
    []      -> Nothing
    v :: vs -> if f v then Just v else lookup f vs

-- object-by-object check: brand-new objects are just "brandNew"
diffRecord : RdapDisplay {} -> DisplayRecord {} -> DisplayRecord Diff
diffRecord orig obj = case lookup (\r -> r.identifier == obj.identifier) orig of
    Nothing     -> using New obj
    Just old    -> let new = diffObject old.object obj.object in { obj | object = new }

diffObject : DisplayObject {} -> DisplayObject {} -> DisplayObject Diff
diffObject orig new = diffed <| Diff.diff orig new

diffed : List (Change (DisplayLine {})) -> DisplayObject Diff
diffed changes = case changes of
    []      -> []
    Added l :: cs -> setDiff New l :: diffed cs
    Removed l :: Added m :: cs -> if l.label == m.label then setDiff Modified m :: diffed cs else diffed (Added m :: cs)
    Removed l :: cs -> diffed cs
    NoChange l :: cs -> setDiff Unchanged l :: diffed cs

setDiff : DiffMode -> DisplayLine a -> DisplayLine Diff
setDiff d a = { label = a.label, value = a.value, display = a.display, diffMode = d }

using : DiffMode -> DisplayRecord {} -> DisplayRecord Diff
using m d = DisplayRecord d.identifier
        <| List.map (setDiff m )
        <| d.object