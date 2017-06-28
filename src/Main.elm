module Main exposing (..)

import Date exposing (fromString, toTime)
import Date.Extra.Compare exposing (Compare2(..), is)
import DOM exposing (target, childNode)
import Either exposing (Either(..))
import Guards exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, value, id, rel, href, src, autofocus)
import Html.Events exposing (onWithOptions, onInput, onClick)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode exposing (Decoder, succeed, string, map)
import List exposing (map, map2)
import List.Extra exposing (last)
import Navigation
import Maybe.Extra exposing (or)
import Platform.Cmd
import Regex
import String
import Task
import Tuple exposing (..)

import Model exposing (..)
import Decode exposing (history)
import Render exposing (viewAsList)

init : Navigation.Location -> ( Model, Cmd Msg )
init loc = let hash = String.dropLeft 1 loc.hash
               resource = if String.isEmpty hash then "203.133.248.0/24" else hash
            in ( Model resource (Left "Searching…") 0 (Nothing, Nothing) (Unlocked, Unlocked) False, search resource )

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


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    Nada ->
        ( model, Cmd.none )
    Fetched f ->
        ( upd { model | response = fromFetch f, selected = 0 }, Cmd.none )
    UrlChange l ->
        init l
    Select i ->
        ( upd { model | selected = i }, Cmd.none )
    StartSearch s ->
        ( model, Navigation.newUrl ("#" ++ s) )
    NavigateDiff direction -> case direction of
                                  Bkwd -> ( navigateBack model, Cmd.none)
                                  Fwd  -> ( navigateForward model, Cmd.none)
    FlipNavLock direction ->
        (flipNavigationLock model direction, Cmd.none)

upd : Model -> Model
upd model =
    let displayedVersions = case Maybe.withDefault [] (versions model) of
                               []            -> (Nothing, Nothing)
                               v :: []       -> (Just v, Nothing)
                               v1 :: v2 :: _ -> (Just v2, Just v1)
            in { model | redraw = not model.redraw, displayedVersions = displayedVersions }

navigateForward : Model -> Model
navigateForward model =
    if (List.length <| Maybe.withDefault [] (versions model)) <= 2 ||
        Maybe.map2 (is Same) (Maybe.map .from <| Maybe.Extra.join <| Maybe.map List.head (versions model))
            (Maybe.map .from <| second model.displayedVersions) == Just True
    then model
    else let versions = Maybe.withDefault [] <| Model.versions model
             (currentLeftVersion, currentRightVersion) = model.displayedVersions
             rightVersion = Maybe.Extra.join <| Maybe.map2 getNextVersion currentRightVersion <| Just versions
             leftVersion = let nextLeftVersion = Maybe.Extra.join <| Maybe.map2 getNextVersion currentLeftVersion <| Just versions
                           in if first model.navigationLocks == Locked || nextLeftVersion == rightVersion
                              then currentLeftVersion
                              else nextLeftVersion
         in { model | displayedVersions = (leftVersion, rightVersion) }

navigateBack : Model -> Model
navigateBack model =
    if (List.length <| Maybe.withDefault [] (versions model)) <= 2 ||
        Maybe.map2 (is Same) (Maybe.map .from <| Maybe.Extra.join <| Maybe.map List.Extra.last (versions model))
            (Maybe.map .from <| first model.displayedVersions) == Just True
    then model
    else let versions = Maybe.withDefault [] <| Model.versions model
             (currentLeftVersion, currentRightVersion) = model.displayedVersions
             leftVersion =  Maybe.Extra.join <| Maybe.map2 getPreviousVersion currentLeftVersion <| Just versions
             rightVersion = let nextRightVersion = Maybe.Extra.join <| Maybe.map2 getPreviousVersion currentRightVersion <| Just versions
                            in if second model.navigationLocks == Locked || nextRightVersion == leftVersion
                               then currentRightVersion
                               else nextRightVersion
         in { model | displayedVersions = (leftVersion, rightVersion) }

flipNavigationLock : Model -> NavigationDirection -> Model
flipNavigationLock model direction =
    let flip state = if state == Locked then Unlocked else Locked
        (bkwdState, fwdState) = model.navigationLocks
    in case direction of
           Fwd  -> {model | navigationLocks = (bkwdState, flip fwdState)}
           Bkwd -> {model | navigationLocks = (flip bkwdState, fwdState)}


-- View

view : Model -> Html Msg
view model = lazy (\z -> view_ model) model.redraw

view_ : Model -> Html Msg
view_ model =
    let body = case model.response of
        Left error     -> [ div [ class "error" ] [ text error ] ]
        Right response -> viewAsList response model.selected model.displayedVersions model.navigationLocks
    in div [ class "main" ] <| List.concat [ styles, (headerBar model), body ]

styles : List (Html a)
styles = [ node "link" [ rel "stylesheet", href "../css/ui.css" ] [] ]

headerBar : Model -> List (Html Msg)
headerBar model =
    [ nav []
        [ ul []
            [ li [] [ img [ class "logo", src "../images/APNIC-Formal-Logo_cmyk-svg-optimized-white.svg" ] []]
            , li [] [ h1 [] [ text "Whowas" ] ]
            , li [] [ searchBox model ]
            ]
        ]
    ]

searchBox : Model -> Html Msg
searchBox model =
    let cease = { stopPropagation = True, preventDefault = True }
    in form [ class "range", onWithOptions "submit" cease searchForm ]
            [ input [ value model.resource, autofocus True ] [] ]

fl : List String -> String
fl xs = String.concat (List.intersperse "\n" xs)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

searchForm : Decoder Msg
searchForm = target (childNode 0 (Json.Decode.map StartSearch (Json.Decode.field "value" string)))

search : String -> Cmd Msg
search resource =
    let typ   = url_of_typ <| infer_type resource
        url   = "//rdap.apnic.net/history/" ++ typ ++ "/" ++ resource
        fetch = Http.toTask <| Http.get url Decode.history
    in fetch |> Task.andThen (\r -> Task.map (\d -> Response d r) Date.now)
             |> Task.attempt Fetched

url_of_typ : ObjectClass -> String
url_of_typ oc = case oc of
    AutNum      -> "autnum"
    Entity      -> "entity"
    Domain      -> "domain"
    InetNum     -> "ip"

-- Infer the RDAP object class of a key
infer_type : String -> ObjectClass
infer_type res
     = String.endsWith ".arpa" res => Domain
    |= Regex.contains (Regex.regex "^AS\\d+$") res => AutNum
    |= Regex.contains (Regex.regex "^([\\d\\.]+|[\\da-fA-F:]+)(/\\d+)?$") res => InetNum
    |= Entity

main : Program Never Model Msg
main = Navigation.program UrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
