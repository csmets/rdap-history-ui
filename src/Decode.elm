module Decode exposing  (history)

import Json.Decode exposing (..)
import Json.Encode exposing (encode)

import Model exposing (..)

history : Decoder (List History)
history = map List.reverse <| list record

record : Decoder History
record = object3 History
    ("handle"    := string)
    ("ipVersion" := string)
    ("versions"  := map List.reverse (list version))

version : Decoder Version
version = object2 Version
    ("applicability" := tuple2 Period string string)
    ("rpsl"          := list string)

prettyPrint : Decoder String
prettyPrint = map (encode 2) value
