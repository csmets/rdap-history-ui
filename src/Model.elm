module Model exposing (Version, History, Identifier, ObjectClass (..), Response, Model, Msg (..), Selected)

import Date exposing (Date)
import Either exposing (Either)
import Http
import Json.Encode exposing (Value)

type alias Response =
    { stamp : Date.Date
    , history : List History
    }

type alias Model =
    { resource : String
    , response : Either String Response
    , selected : Int
    , redraw : Bool
    }

type Msg
    = Nada
    | Fetched (Result Http.Error Response)
    | StartSearch String
    | Select Int

type Selected
    = Selected
    | NotSelected

-- The supported object classes
type ObjectClass
    = InetNum
    | AutNum
    | Entity
    | Domain

type alias Identifier =
    { objectClass   : ObjectClass
    , handle        : String
    }

-- The history of one object
type alias History =
    { identifier    : Identifier
    , versions      : List Version
    }

-- One version of one object
type alias Version =
    { from : Date
    , until : Maybe Date
    , object : Value
    }
