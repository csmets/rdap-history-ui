module Model exposing (Version, History, Identifier, ObjectClass (..), Response, Model, Msg (..), Selected
                      , history, versions)

import Date exposing (Date)
import Either exposing (Either)
import Http
import Json.Encode exposing (Value)
import Keyboard
import Navigation exposing (Location)
import List.Extra exposing ((!!))

type alias Response =
    { stamp : Date.Date  -- TODO: this can probably be removed
    , history : List History
    }

type alias Model =
    { resource : String
    , response : Either String Response
    , selected : Int
    , viewModification : Maybe Date
    , redraw : Bool
    }

type Msg
    = Nada -- TODO remove this?
    | UrlChange Location
    | Fetched (Result Http.Error Response)
    | StartSearch String
    | Select Int
    | NavigateDiffForward
    | NavigateDiffBack
    | KeyMsg Keyboard.KeyCode

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


-- Utility methods
history : Model -> Maybe History
history model = Maybe.andThen ( flip (!!) model.selected) <| Maybe.map .history <| Either.toMaybe model.response

versions : Model -> Maybe (List Version)
versions = Maybe.map .versions << history
