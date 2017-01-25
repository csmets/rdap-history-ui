module Model exposing (Version, History, ObjectClass (..), Response, Model, Msg (..), Selected)

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

-- The history of one object
type alias History =
    { objectClass   : ObjectClass
    , handle        : String
    , versions      : List Version
    }

-- One version of one object
type alias Version =
    { from : Date
    , until : Maybe Date
    , object : Value
    }
