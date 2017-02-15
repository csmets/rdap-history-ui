module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, value, id, rel, href)
import Html.Events exposing (onWithOptions, onInput, onClick)
import Html.Lazy exposing (lazy)
import Http
import Task
import Platform.Cmd
import List exposing (map, map2)
import String
import Either exposing (Either(..))
import Date exposing (fromString, toTime)
import DOM exposing (target, childNode)
import Json.Decode exposing (Decoder, succeed, string, map)

import Model exposing (..)
import Decode exposing (history)
import Render exposing (viewAsList)

init : String -> ( Model, Cmd Msg )
init resource =
    ( Model resource (Left "Loading…") 0 False, search resource )

errMsg : Http.Error -> String
errMsg err = case err of
    Http.Timeout ->
        "timeout"
    Http.NetworkError ->
        "network error"
    Http.BadStatus r ->
        "unexpected status"
    Http.BadPayload i r ->
        "bad response " ++ i
    Http.BadUrl _ ->
        "invalid URL"

fromFetch : Result Http.Error Response -> Either String Response
fromFetch r = case r of
    Ok ok -> Right ok
    Err e -> Left (errMsg e)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    Nada ->
        ( model, Cmd.none )
    Fetched f ->
        ( upd { model | response = fromFetch f, selected = 0 }, Cmd.none )
    -- Error err ->
    --     ( upd { model | response = Left (errMsg err), selected = 0 }, Cmd.none )
    -- Success h ->
    --     ( upd { model | response = Right h, selected = 0 }, Cmd.none )
    Select i ->
        ( upd { model | selected = i }, Cmd.none )
    StartSearch s ->
        ( upd { model | response = Left "Searching…", resource = s }, search s )

upd : Model -> Model
upd model = { model | redraw = not model.redraw }

view : Model -> Html Msg
view model = lazy (\z -> view_ model) model.redraw

view_ : Model -> Html Msg
view_ model =
    let body = case model.response of
        Left error     -> [ div [ class "error" ] [ text error ] ]
        Right response -> viewAsList response model.selected
    in div [ class "main" ] <| List.concat [ styles, (headerBar model), body ]

styles : List (Html a)
styles = [ node "link" [ rel "stylesheet", href "../css/ui.css" ] [] ]

headerBar : Model -> List (Html Msg)
headerBar model =
    [ nav []
        [ ul []
            [ li [] [ h1 [] [ text "Historical RDAP" ] ]
            , li [] [ searchBox model ]
            ]
        ]
    ]

searchBox : Model -> Html Msg
searchBox model =
    let cease = { stopPropagation = True, preventDefault = True }
    in form [ class "range", onWithOptions "submit" cease searchForm ]
            [ input [ value model.resource ] [] ]

fl : List String -> String
fl xs = String.concat (List.intersperse "\n" xs)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

searchForm : Decoder Msg
searchForm = target (childNode 0 (Json.Decode.map StartSearch (Json.Decode.field "value" string)))

search : String -> Cmd Msg
search resource =
    let url   = "//rdap.apnic.net/history/ip/" ++ resource
        fetch = Http.toTask <| Http.get url history
    in fetch |> Task.andThen (\r -> Task.map (\d -> Response d r) Date.now)
             |> Task.attempt Fetched

main : Program Never Model Msg
main = program
    { init = init "203.133.248.0/24"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
