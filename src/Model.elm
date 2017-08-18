module Model exposing (Version, History, Identifier, ObjectClass (..), Response, Model, Msg (..), Selected
                      , history, versions, LockerState (..), NavigationDirection (..), getNextVersion
                      , getPreviousVersion, getDistance, canNavigate)

import Date exposing (Date, toTime)
import Date.Extra.Compare exposing (Compare2(..), is)
import Either exposing (Either)
import Http
import Json.Encode exposing (Value)
import Maybe exposing (map, map2, map3, withDefault)
import Maybe.Extra exposing (join)
import Navigation exposing (Location)
import List exposing (head)
import List.Extra exposing ((!!), last)

type alias Response =
    { stamp : Date.Date
    , history : List History
    }

type alias Model =
    { resource : String
    , response : Either String Response
    , selected : Int
    , displayedVersions : (Maybe Version, Maybe Version)
    , navigationLocks : (LockerState, LockerState)
    , versionDateDetail : Maybe Date
    , redraw : Bool
    }

type Msg
    = Nada
    | UrlChange Location
    | Fetched (Result Http.Error Response)
    | StartSearch String
    | Select Int
    | NavigateDiff NavigationDirection
    | NavigateDiffToVersion Version
    | FlipNavLock NavigationDirection
    | FlipShowVersionDateDetail (Maybe Date)

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

type NavigationDirection = Fwd | Bkwd

type LockerState = Locked | Unlocked

-- Utility methods
history : Model -> Maybe History
history model = Maybe.andThen (flip (!!) model.selected) <| Maybe.map .history <| Either.toMaybe model.response

versions : Model -> Maybe (List Version)
versions = Maybe.map .versions << history

getNextVersion : Version -> List Version -> Maybe Version
getNextVersion current = List.Extra.last << List.Extra.takeWhile (\v -> is After v.from current.from)

getPreviousVersion : Version -> List Version -> Maybe Version
getPreviousVersion current = List.head << List.Extra.dropWhile (\v -> is SameOrAfter v.from current.from)

getDistance : Version -> Version -> List Version -> Maybe Int
getDistance v1 v2 vs = Maybe.map2 (-) (List.Extra.elemIndex v1 vs) (List.Extra.elemIndex v2 vs)

canNavigate : List Version -> (Maybe Version, Maybe Version) -> NavigationDirection -> (LockerState, LockerState) -> Bool
canNavigate vs (leftVersion, rightVersion) dir (bkwdState, fwdState) =
    let displayedVersions = Maybe.Extra.values [leftVersion, rightVersion]
        finalVersion = if dir == Fwd then head vs else last vs
        displayed = (if dir == Fwd then last else head) displayedVersions
        isOnlyDirectionLocked = case dir of
                                    Fwd  -> fwdState == Locked && bkwdState == Unlocked
                                    Bkwd -> bkwdState == Locked && fwdState == Unlocked
        isNotOnEdge = withDefault False <| map ((flip (>)) 0) <| map abs <| join <|
                       map3 (getDistance) finalVersion displayed (Just vs)
        leftAndRightAreNotAdjacent = withDefault False <| map ((flip (>)) 1) <| map abs <| join <|
                                  map3 (getDistance) leftVersion rightVersion (Just vs)
    in if isOnlyDirectionLocked
       then leftAndRightAreNotAdjacent || isNotOnEdge
       else isNotOnEdge
