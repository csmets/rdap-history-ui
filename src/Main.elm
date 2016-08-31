import Html exposing (..)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onWithOptions, onInput, onClick)
import Html.App as App
import Html.Lazy exposing (lazy)
import Http
import Task
import String
import Either exposing (Either (..))
import RPSL exposing (..)
import Diff exposing (diffLines, Change(..))
import DOM exposing (target, childNode)
import Json.Decode exposing (Decoder, succeed, (:=), string, object1)

import Model exposing (..)
import Decode exposing (history)

type alias Model =
    { resource : String
    , searchString : String
    , result   : Either String (List (History))
    , selected : Int
    , redraw   : Bool }

type Msg
  = Nada
  | Error Http.Error
  | Success (List History)
  | UpdateRange String
  | StartSearch String
  | Select Int

type Selected = Selected | NotSelected

init : String -> (Model, Cmd Msg)
init resource = (Model resource "" (Left "Loading") 0 False, search resource)

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
        Error err     -> (upd {model | result = Left (errMsg err), selected = 0}, Cmd.none)
        Success h     -> (upd {model | result = Right h, selected = 0}, Cmd.none)
        UpdateRange s -> (    {model | searchString = s}, Cmd.none)
        Select i      -> (upd {model | selected = i}, Cmd.none)
        StartSearch s -> (upd {model | result = Left "Searching…", resource = s }, search s)

upd : Model -> Model
upd model = { model | redraw = not model.redraw }

view : Model -> Html Msg
view model = lazy (\z -> view' model) model.redraw

view' : Model -> Html Msg
view' model =
    let
        body = case model.result of
            Left error      -> [ div [ class "error" ] [text error] ]
            Right history   -> viewAsList history model.selected
        cease = { stopPropagation = True, preventDefault = True }
    in
        div [ class "main" ] <|
            [ h1 [] [text "APNIC registry history search"]
            , form [ class "range", onWithOptions "submit" cease searchForm ]
                [ input [ value model.resource ] [] ]
            ] `List.append` body

viewAsList : List History -> Int -> List (Html Msg)
viewAsList hs idx = [ div [ class "historyPane" ]
    [ ol [ class "objectList" ] <| List.indexedMap (viewSummary idx) hs
    , div [ class "detail", id "content" ] [ firstVersion <| List.head (List.drop idx hs) ]
    ] ]

firstVersion : Maybe History -> Html Msg
firstVersion mh = case mh of
    Nothing -> text ""
    Just h  -> viewVersions h

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
viewVersions h =div [ class "versions" ] (
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
        Task.perform Error Success (Http.get history url)

main : Program Never
main = App.program
    { init = init "203.133.248.0/24"
    , view = view
    , update = update
    , subscriptions = subscriptions }
