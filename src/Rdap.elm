module Rdap exposing (objectClass, handle, render, output, diff)

-- The renderers in this module are quite specific to APNIC's use profile of RDAP.
-- Pull requests making the rendering less rigidly fixed to one profile are very welcome.

import Dict             exposing (Dict)
import Diff             exposing (Change(..))
import Html             exposing (..)
import Html.Attributes  exposing (class, colspan, href)
import Json.Decode      exposing (..)
import Json.Encode
import List             exposing (foldr, concat, intersperse, filter)
import List.Extra       exposing (groupWhile, findIndex, (!!), removeAt, find, remove)
import Maybe            exposing (Maybe)
import Result
import String           exposing (words, lines, split)

import Model exposing (..)

type alias Remark = { title : String , description : List String }
type alias VCardEntry =
    { name : String
    , parameters : Dict String Value
    , kind : String
    , content : Value
    }

-- A structure to represent an RDAP object interpreted for display
type DisplayMode = Text | Lookup | Preformatted
type DisplayValue = Value String | ModifiedValue (List (String, DiffMode))
type alias DisplayLine a = { a | label : String, value : DisplayValue, display : DisplayMode }
type alias DisplayObject a = List (DisplayLine a)
type alias DisplayRecord a = { identifier : Identifier, object : DisplayObject a }
type alias RdapDisplay a = List (DisplayRecord a)

-- Annotating a display line with a diff state
type DiffMode = Unchanged | Modified | New | Deleted
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
    AutNum  -> { identifier = i, object = autnum value  } :: entities value
    Entity  -> { identifier = i, object = entity Text value  } :: entities value
    Domain  -> { identifier = i, object = domain value  } :: entities value

-- Extract all the entities linked in an RDAP object
entities : Value -> RdapDisplay {}
entities v = field "entities" (list value)
        |> Json.Decode.map (List.concatMap mapObject)
        |> \d -> Result.withDefault [] (decodeValue d v)

mapObject : Value -> RdapDisplay {}
mapObject v = identifier v
        |> Maybe.map (\i -> [{ identifier = i, object = entity Lookup v }])
        |> Maybe.withDefault []

    -- like `text`, but will produce interspersed text and <br>
newlined : DisplayValue -> List (Html a)
newlined dv =
    case dv of
        Value s           -> split "\n" s |> List.map text |> List.intersperse (br [] [])
        ModifiedValue mvs -> List.map (\(s,d) -> if (s == "\n")
                                                 then br [class <| modifiedWordClass d] []
                                                 else convertModifiedValue (s,d)) mvs

output : RdapDisplay Diff -> Html Msg
output rdap = ul [class "rdap-display"]
        <| List.concat
        <| List.intersperse [spacer]
        <| List.map (.object >> object) rdap

object : DisplayObject Diff -> List (Html Msg)
object lines = List.map line lines

line : DisplayLine Diff -> Html Msg
line { label, value, display, diffMode } = case display of
    Text         -> row diffMode (text label) (newlined value)
    Lookup       -> row diffMode (text label) [ a [ href ("#" ++ flatText value) ] (convertValue value) ]
    Preformatted -> row diffMode (text label) [ pre [] (convertValue value) ]

flatText : DisplayValue -> String
flatText dv =
    case dv of
        Value s           -> s
        ModifiedValue mvs -> String.concat <| intersperse " " <| List.map (\(s,d) -> s) mvs

convertValue : DisplayValue -> List (Html a)
convertValue dv =
    case dv of
        Value s           -> [text s]
        ModifiedValue mvs -> List.map convertModifiedValue mvs

convertModifiedValue : (String, DiffMode) -> Html a
convertModifiedValue (s, dm) =
    case dm of
        Unchanged -> text s
        _         -> span [class <| modifiedWordClass dm] [text s]

modifiedWordClass : DiffMode -> String
modifiedWordClass dm =
    case dm of
        New     -> "diff-word-new"
        Deleted -> "diff-word-deleted"
        _       -> ""

row : DiffMode -> Html a -> List (Html a) -> Html a
row d l r = li [ class <| "rdap-row " ++ (diffClass d) ] [ div [ class "rdap-label" ] [ l ],
                                                            div [ class "rdap-value" ] r ]

diffClass : DiffMode -> String
diffClass d = case d of
    Unchanged   -> "diff-unchanged"
    Modified    -> "diff-modified"
    New         -> "diff-new"
    Deleted     -> "diff-deleted"

spacer : Html a
spacer = li [ class "spacer" ] [ hr [] []  ]

{- Functions to assist rendering into an RdapDisplay -}

display : DisplayMode -> String -> String -> DisplayLine {}
display d l v = { label = l, value = Value v, display = d }

run : (a -> List b) -> Decoder a -> Value -> List b
run f d v = Result.withDefault [] <| Result.map f (decodeValue d v)

labelled : String -> Decoder String -> Value -> DisplayObject {}
labelled k d v = (run <| \h -> [ display Text k h ]) d v

mode : DisplayMode -> String -> Decoder String -> Value -> DisplayObject {}
mode m k d v = (run <| \h -> [ display m k h ]) d v

tabulated : Decoder (List (DisplayLine a)) -> Value -> DisplayObject a
tabulated = run identity

remark : Remark -> DisplayLine {}
remark r = { label = r.title, value = Value <| String.join "\n" r.description, display = Preformatted }

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

entity : DisplayMode -> Value -> DisplayObject {}
entity d v = List.concatMap (\i -> i v)
    [ mode d   "handle"         (field "handle"   string)
    , labelled "country"        (field "country"  string)
    , vcard                     (field "vcardArray" <| index 1 <| list decodeVcard)
    , labelled "roles"          (field "roles"     string)
    , tabulated                 (field "remarks"  (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices"  (Json.Decode.map (List.map remark) remarks))
    ]

-- Omitted: status, start, end, links, events, port43
autnum : Value -> DisplayObject {}
autnum v = List.concatMap (\i -> i v)
    [ labelled "handle"         (field "handle" string)
    , labelled "AS name"        (field "name" string)
    , labelled "country"        (field "country"  string)
    , labelled "type"           (field "type"     string)
    , tabulated                 (field "remarks"  (Json.Decode.map (List.map remark) remarks))
    , tabulated                 (field "notices"  (Json.Decode.map (List.map remark) remarks))
    ]

-- Omitted: variants, ldhName, unicodeName, secureDNS, status, publicIds, network, links, events, port43
domain : Value -> DisplayObject {}
domain v = List.concatMap (\i -> i v)
    [ labelled "handle"         (field "handle" string)
    , labelled "servers"        (field "nameservers" <| Json.Decode.map (String.join "\n") (list (field "ldhName" string)))
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
        |> Result.map (\a -> [ display Text "address" a ])
        |> Result.withDefault []

simple : String -> VCardEntry -> DisplayObject {}
simple l v = run (\n -> [ display Text l n ]) string v.content

-- The "tel" vCard spec is a complex piece of work, this function will only recognise TYPE=fax, and only if it's the
-- only type parameter: ["work","fax"] will be considered a voice number, for instance.
tel : VCardEntry -> DisplayObject {}
tel v = Dict.get "type" v.parameters
        |> Result.fromMaybe "unused error description"
        |> Result.andThen (decodeValue string)
        |> Result.withDefault "no type parameter"
        |> \t -> (if t /= "fax" then "voice" else "fax")
        |> \t -> run (\n -> [ display Text t n ]) string v.content

-- This is only a partial decode of jCard attributes
vcardEntry : VCardEntry -> DisplayObject {}
vcardEntry v = case v.name of
    "fn"    -> simple "name" v
    "kind"  -> simple "kind" v
    "adr"   -> adr v
    "tel"   -> tel v
    "email" -> simple "email" v
    -- the default is to show raw JSON data; could do better
    _       -> run (\raw -> [ display Text v.name (Json.Encode.encode 0 raw) ]) value v.content

{- Diffs between two RdapDisplays -}
diff : Maybe (RdapDisplay {}) -> RdapDisplay {} -> RdapDisplay Diff
diff mOrig new = case mOrig of
    Nothing     -> List.map (using Unchanged) new
    Just orig   -> (List.map (diffRecord orig) new) ++
                        (List.map (using Deleted)
                                  (filter (\r -> find (\r2 -> r2.identifier == r.identifier) new == Nothing) orig))

-- object-by-object check: brand-new objects are just "brandNew"
diffRecord : RdapDisplay {} -> DisplayRecord {} -> DisplayRecord Diff
diffRecord orig obj = case find (\r -> r.identifier == obj.identifier) orig of
    Nothing     -> using New obj
    Just old    -> let new = diffObject old.object obj.object in { obj | object = new }

diffObject : DisplayObject {} -> DisplayObject {} -> DisplayObject Diff
diffObject orig new = diffed <| Diff.diff orig new

diffed : List (Change (DisplayLine {})) -> DisplayObject Diff
diffed changes = case changes of
    []              -> []
    Added l :: cs   -> setDiff New l :: diffed cs
    Removed l :: cs ->
        -- this is required since Diff.diff groups adjacent "Removed" entries
        let mc = find (\c -> case c of
                                 Added l2 -> l.label == l2.label
                                 _        -> False
                      ) cs
        in case mc of
               Nothing -> setDiff Deleted l :: diffed cs
               Just (Added c) -> setModifiedDiff l c :: diffed (remove (Added c) cs)
               _              -> setDiff Deleted l :: diffed cs
    NoChange l :: cs -> setDiff Unchanged l :: diffed cs

setDiff : DiffMode -> DisplayLine a -> DisplayLine Diff
setDiff d a = { label = a.label, value = a.value, display = a.display, diffMode = d }

setModifiedDiff : DisplayLine a -> DisplayLine a -> DisplayLine Diff
setModifiedDiff from to =
    let splitValue = concat << intersperse ["\n"] << List.map splitLine << lines
        splitLine = List.map String.fromList << groupWhile (\c1 c2 -> if c1 == ' ' then c1 == c2 else c2 /= ' ')
                        << String.toList
        fromString = flatText from.value
        toString   = flatText to.value
        wordsDiff = Diff.diff (splitValue fromString) (splitValue toString)
        convertDiff d = case d of
                            Added w    -> (w, New)
                            Removed w  -> (w, Deleted)
                            NoChange w -> (w, Unchanged)
        -- convert "non-modified" whitespaces between two modified words to "modified" as well for aesthetic purposes
        sanatiseWS cs = case cs of
                            [] -> []
                            NoChange w1 :: NoChange w2 :: c :: rem ->
                                NoChange w1 :: NoChange w2 :: (sanatiseWS <| c :: rem)
                            c :: NoChange w1 :: NoChange w2  :: rem ->
                                c :: NoChange w1 :: NoChange w2 :: (sanatiseWS rem)
                            c1 :: NoChange w :: c2 :: rem ->
                                if (String.all ((==) ' ') w)
                                then c1 :: Added w :: Removed w :: (sanatiseWS <| c2 :: rem)
                                else c1 :: NoChange w :: (sanatiseWS <| c2 :: rem)
                            c :: rem ->
                                c :: (sanatiseWS rem)
        modValue = foldr (\d ds -> convertDiff d :: ds) [] <| sanatiseWS  <| wordsDiff
    in { label = to.label, value = ModifiedValue modValue, display = to.display, diffMode = Modified }

using : DiffMode -> DisplayRecord {} -> DisplayRecord Diff
using m d = DisplayRecord d.identifier
        <| List.map (setDiff m )
        <| d.object
