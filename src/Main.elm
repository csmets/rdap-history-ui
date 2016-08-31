import Html exposing (..)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onWithOptions, onInput, onClick)
import Html.App as App
import Html.Lazy exposing (lazy)
import Http
import Task
import String
import Either exposing (Either (..))
import Result exposing (map)
import RPSL exposing (..)
import Diff exposing (diffLines, Change(..))
import Date exposing (fromString, toTime)
import DOM exposing (target, childNode)
import Json.Decode exposing (Decoder, succeed, (:=), string, object1)

import Model exposing (..)
import Decode exposing (history)

type alias Response =
    { stamp   : Date.Date
    , history : List History }

type alias Model =
    { resource : String
    , response : Either String Response
    , selected : Int
    , redraw   : Bool }

type Msg
  = Nada
  | Error Http.Error
  | Success Response
  | StartSearch String
  | Select Int

type Selected = Selected | NotSelected

init : String -> (Model, Cmd Msg)
init resource = (Model resource (Left "Loading…") 0 False, search resource)

errMsg : Http.Error -> String
errMsg err =
    case err of
        Http.Timeout             -> "timeout"
        Http.NetworkError        -> "network error"
        Http.UnexpectedPayload s -> "unexpected payload " ++ s
        Http.BadResponse i s     -> "bad response " ++ s

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Nada          -> (     model, Cmd.none)
        Error err     -> (upd {model | response = Left (errMsg err), selected = 0}, Cmd.none)
        Success h     -> (upd {model | response = Right h, selected = 0}, Cmd.none)
        Select i      -> (upd {model | selected = i}, Cmd.none)
        StartSearch s -> (upd {model | response = Left "Searching…", resource = s }, search s)

upd : Model -> Model
upd model = { model | redraw = not model.redraw }

view : Model -> Html Msg
view model = lazy (\z -> view' model) model.redraw

view' : Model -> Html Msg
view' model =
    let
        body = case model.response of
            Left error      -> [ div [ class "error" ] [text error] ]
            Right response  -> viewAsList response model.selected
    in
        div [ class "main" ] <|
            headerBar model `List.append` body

headerBar : Model -> List (Html Msg)
headerBar model =
    [nav [] [ul []
        [ li [] [h1 [] [text "WHOWAS (prototype)" ]]
        , li [] [ searchBox model ] ] ] ]

searchBox : Model -> Html Msg
searchBox model = let cease = { stopPropagation = True, preventDefault = True } in
    form [ class "range", onWithOptions "submit" cease searchForm ]
        [ input [ value model.resource ] [] ]

viewAsList : Response -> Int -> List (Html Msg)
viewAsList response idx = [ div [ class "historyPane" ]
    [ ol [ class "objectList" ] <| List.indexedMap (viewSummary idx) response.history
    , div [ class "detail", id "content" ]
        (firstVersion response.stamp <| List.head (List.drop idx response.history))
    ] ]

firstVersion : Date.Date -> Maybe History -> List (Html Msg)
firstVersion now mh = case mh of
    Nothing -> [ text "" ]
    Just h  -> [viewTimeline now h, viewVersions h]

viewTimeline : Date.Date -> History -> Html Msg
viewTimeline now h =
    let
        asTime   s = Date.toTime <| Result.withDefault now <| Date.fromString s
        timespan v = { whence = asTime v.applicability.whence
                     , until  = asTime v.applicability.until }
        times = List.map timespan h.versions
        minmax = List.concatMap (\p -> [p.whence, p.until]) times
        earliest = List.minimum minmax
        latest   = List.maximum minmax
    in
        div [ class "timeline" ] [
            text (toString (Maybe.map Date.fromTime earliest, Maybe.map Date.fromTime latest, now))
        ]

viewSummary : Int -> Int -> History -> Html Msg
viewSummary sel idx h = li
    [ class (if sel == idx then "selected" else "")
    , onClick (Select idx) ]
    [ span [ class "handle" ] [ text h.handle ] ]

viewHistory : List History -> List (Html Msg)
viewHistory = List.map viewObject

viewObject : History -> Html Msg
viewObject h =
    div [ class "object" ]
        [ span [ class "range" ] [ text h.handle ]
        , viewVersions h
        , div [ class "clearfix" ] []
        ]

viewVersions : History -> Html Msg
viewVersions h = div [ class "versions" ] (
            viewFirst h.versions :: List.map2 viewVersion h.versions (List.drop 1 h.versions))


viewFirst : List Version -> Html Msg
viewFirst vs = case List.head vs of
    Just h  -> div [ class "version" ]
            [ viewPeriod h.applicability
            , div [ class "diff" ] [ viewChange <| NoChange <| fl h.rpsl ]
            ]
    Nothing -> div [ class "version" ] []

viewDiff : Version -> Version -> Html Msg
viewDiff v1 v2 = div [ class "diff" ]
    <| List.map viewChange (diffLines (fl v1.rpsl) (fl v2.rpsl))

fl : List String -> String
fl xs = String.concat (List.intersperse "\n" xs)

viewPeriod : Period -> Html Msg
viewPeriod p = div [] [text <| "From " ++ p.whence ++ " until " ++ p.until]

viewChange : Change -> Html Msg
viewChange c = case c of
    NoChange s  -> span [ class "nochange" ] [text s]
    Changed f t -> span [ class "changed"  ] [text t]
    Added    s  -> span [ class "added"    ] [text s]
    Removed  s  -> div  [ class "removed"  ] []

viewVersion : Version -> Version -> Html Msg
viewVersion v1 v2 =
    div [ class "version" ]
        [ viewPeriod v2.applicability
        , viewDiff v1 v2
        ]

viewAttribute : RpslAttr -> Html Msg
viewAttribute (RpslAttr k v) = text (k ++ ": " ++ v ++ "\n")

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

searchForm : Decoder Msg
searchForm = target (childNode 0 (object1 StartSearch ("value" := string)))

search : String -> Cmd Msg
search resource =
    let
        url = "http://localhost:8080/v4?range=" ++ resource
    in
        Task.perform Error Success <|
            Task.map2 Response Date.now (Http.get history url)

main : Program Never
main = App.program
    { init = init "203.133.248.0/24"
    , view = view
    , update = update
    , subscriptions = subscriptions }
