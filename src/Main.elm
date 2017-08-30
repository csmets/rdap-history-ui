module Main exposing (..)

import Date exposing (Date, fromString, toTime)
import DOM exposing (target, childNode)
import Either exposing (Either(..))
import Guards exposing (..)
import Html exposing (nav, div, node, Html, li, h1, text, img, form, input, ul, span, button)
import Html.Attributes exposing (class, value, id, rel, href, src, autofocus)
import Html.Events exposing (onWithOptions, onInput, onClick)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode exposing (Decoder, succeed, string)
import List exposing (head, reverse)
import Navigation
import Maybe exposing (withDefault, map, map2)
import Maybe.Extra exposing (or, join)
import Platform.Cmd
import Regex
import String
import Task
import Tuple2

import Icons exposing (..)
import Model exposing (..)
import Decode exposing (history)
import Render exposing (viewAsList)

init : Navigation.Location -> ( Model, Cmd Msg )
init loc = let hash = String.dropLeft 1 loc.hash
               resource = if String.isEmpty hash then "203.133.248.0/24" else hash
            in ( Model resource (Left "Searching…") 0 (Nothing, Nothing) (Unlocked, Unlocked) Nothing False Lifetime Nothing,
                     search resource )

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
        ( upd { model | selected = i, navigationLocks = (Unlocked, Unlocked) }, Cmd.none )
    StartSearch s ->
        ( model, Navigation.newUrl ("#" ++ s) )
    NavigateDiff direction ->
        ( navigate model direction, Cmd.none )
    NavigateDiffToVersion v ->
        ( navigateToVersion model v, Cmd.none )
    FlipNavLock direction ->
        (flipNavigationLock model direction, Cmd.none)
    FlipShowVersionDateDetail d ->
        (flipShowVersionDateDetail model d, Cmd.none)
    ZoomTimelineWidget z d ->
        (zoomTimelineWidget model z d, Cmd.none)

upd : Model -> Model
upd model =
    let displayedVersions = case withDefault [] (versions model) of
                               []            -> (Nothing, Nothing)
                               v :: []       -> (Nothing, Just v)
                               v1 :: v2 :: _ -> (Just v2, Just v1)
            in { model | redraw = not model.redraw, displayedVersions = displayedVersions, versionDateDetail = Nothing }

navigate : Model -> NavigationDirection -> Model
navigate model dir =
    if not <| canNavigate (withDefault [] <| versions model) model.displayedVersions dir model.navigationLocks
    then model
    else let versions = Model.versions model
             (v1, v2) = (if dir == Fwd then identity else Tuple2.swap) model.displayedVersions
             (l1, l2) = (if dir == Fwd then identity else Tuple2.swap) model.navigationLocks
             getAdjacent = if (dir == Fwd) then getNextVersion else getPreviousVersion
             v1AndV2Adjacent = (join <| map2 getAdjacent v1 versions) == v2
             newV2 = if (l2 == Unlocked) || (l1 == l2) || ((l2 == Locked) && v1AndV2Adjacent)
                            then join <| map2 getAdjacent v2 versions
                            else v2
             newV1 = let nextV1 = join <| map2 getAdjacent v1 versions
                           in if (l1 == Locked && l2 == Unlocked) || nextV1 == newV2
                              then v1
                              else nextV1
             newDV = (if dir == Fwd then identity else Tuple2.swap) (newV1, newV2)
         in { model | displayedVersions = newDV , versionDateDetail = Nothing }

navigateToVersion : Model -> Version -> Model
navigateToVersion  model version =
    let versions = withDefault [] <| Model.versions model
        (v1, v2) = model.displayedVersions
        (l1, l2) = model.navigationLocks
        newDV = (l1 == Unlocked && l2 == Unlocked) => (previousV, Just version)
             |= (l1 == Locked && l2 == Locked)     => (v1, v2)
             |= (l1 == Locked && (withDefault 0 distanceToV1) > 0) => (v1, Just version)
             |= (l2 == Locked && (withDefault -1 distanceToV2) <= 0) => (previousV, v2)
             |= (v1, v2)
        distanceToV1 = join <| Maybe.map3 Model.getDistance v1 (Just version) (Just versions)
        distanceToV2 = join <| Maybe.map3 Model.getDistance v2 (Just version) (Just versions)
        previousV = getPreviousVersion version versions
    in { model | displayedVersions = newDV , versionDateDetail = Nothing }

flipNavigationLock : Model -> NavigationDirection -> Model
flipNavigationLock model direction =
    let flip state = if state == Locked then Unlocked else Locked
        (bkwdState, fwdState) = model.navigationLocks
        newstate = if direction == Fwd then flip fwdState else flip bkwdState
        (newBkwdState, newFwdState) = case direction of
                                          Fwd  -> (bkwdState, flip fwdState)
                                          Bkwd -> (flip bkwdState, fwdState)
        (leftVersion, rightVersion) = model.displayedVersions
        newLeftVersion = case newstate of
                              Unlocked -> join <| map2 getPreviousVersion rightVersion (versions model)
                              Locked   -> leftVersion
    in {model | navigationLocks = (newBkwdState, newFwdState), displayedVersions = (newLeftVersion, rightVersion),
                                  versionDateDetail = Nothing}

flipShowVersionDateDetail : Model -> Maybe Date -> Model
flipShowVersionDateDetail m d = { m | versionDateDetail = d }

zoomTimelineWidget : Model -> TimelineZoom -> Maybe Date -> Model
zoomTimelineWidget m z d = {m | timelineWidgetZoom = z, timelineWidgetZoomDate = d}

-- View

view : Model -> Html Msg
view model = lazy (\z -> view_ model) model.redraw

view_ : Model -> Html Msg
view_ model =
    let body = case model.response of
        Left error     -> [ div [ class "error" ] [ text error ] ]
        Right response -> viewAsList response model
    in div [ class "main" ] <| List.concat [ styles, (headerBar model), body ]

styles : List (Html a)
styles = [ node "link" [ rel "stylesheet", href "css/ui.css" ] [] ]

headerBar : Model -> List (Html Msg)
headerBar model =
    [ div [class "headerBar"]
          [ div [ class "branding" ] [ span [class "title"] [ text "Whowas" ] ]
          , div [] [ searchBox model ]
          ]
    ]

searchBox : Model -> Html Msg
searchBox model =
    let cease = { stopPropagation = True, preventDefault = True }
    in form [ class "range", onWithOptions "submit" cease searchForm ]
            [ input [ value model.resource, autofocus True ] [],
              button [class "searchButton"] [zoomIcon "searchIcon"]]

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
