module Render exposing (viewAsList)

import Date exposing (Date)
import Date.Extra.Compare exposing (Compare2(..), is)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format exposing (formatUtc, isoDateFormat)
import Html exposing (..)
import Html.Attributes exposing (class, value, id, title, disabled, style, src)
import Html.Events exposing (onWithOptions, onInput, onClick)
import List exposing (length)
import List.Extra exposing ((!!), dropWhile, last)
import Maybe.Extra exposing (isJust, (?))
import Svg exposing (svg, path)
import Svg.Attributes exposing (width, height, viewBox, strokeLinecap, strokeLinejoin, strokeWidth, fill)

import Model exposing (..)
import Rdap

-- Rendering happens inside a context
type alias Context = {
    history : History,
    wasVersion : Maybe Version,
    isVersion : Maybe Version,
    versions : List Version,
    modificationDate : Date
}

mkCtx : History -> Date -> Context
mkCtx h modDate =
    let split = dropWhile (\v -> is After v.from modDate) h.versions
        isVersion = split !! 0
        wasVersion = split !! 1
        versions = Maybe.Extra.values [wasVersion, isVersion]
    in Context h wasVersion isVersion versions modDate

viewAsList : Response -> Int -> Date -> List (Html Msg)
viewAsList response idx modDate =
    let history = response.history !! idx
    in case history of
           Nothing -> [] -- TODO: will never happen. deal with it differently?
           Just h -> let ctx = mkCtx h modDate
                     in [div [class "historyPane"] ((objectListPanel response.history idx) ++ (detailPanel ctx))]

-- Selection Panel

objectListPanel : List History -> Int -> List (Html Msg)
objectListPanel history idx =
    if length history <= 1
    then []
    else [ol [ class "objectList" ] <| List.indexedMap (viewSummary idx) history]

viewSummary : Int -> Int -> History -> Html Msg
viewSummary sel idx h = li
        [ class (if sel == idx then "selected" else "" )
        , onClick (Select idx) ]
        [ span [ class "handle" ] [ text h.identifier.handle ] ]

-- Detail Panel

detailPanel : Context -> List (Html Msg)
detailPanel ctx =
    [div [class "detailPanel"] [
        navPanel ctx Left,
        div [class "detailCenterPanel"] [
          versionDatesPanel ctx.versions,
          div [class "diffPanel"] [diffPanel ctx]
        ],
        navPanel ctx Right
    ]]

diffPanel : Context -> Html Msg
diffPanel ctx =
    let paired   = List.map2 (,) (Nothing :: List.map Just ctx.versions) ctx.versions
    in div [ class "versions" ] (List.map (uncurry (viewVersion ctx)) paired)

versionDatesPanel : List Version -> Html Msg
versionDatesPanel vs =
    case vs of
        []            -> text ""
        v :: []       -> div [class "versionDatesPanel"] [
                                   div [] ((createDateLabel (Just v.from)) ++ [text " >"]),
                                   div [] ([text "< "] ++ createDateLabel v.until)
                                ]
        v1 :: v2 :: _ -> div [class "versionDatesPanel"] [
                                   div [class "versionDateLeft"] [span []
                                                                      ((createDateLabel (Just v1.from)) ++ [text " >"])],
                                   div [class "versionDateCenter"] [span [] ([text "< "] ++
                                                                        createDateLabel (Just v2.from) ++ [text " >"])],
                                   div [class "versionDateRight"] [span [] ([text "< "] ++ createDateLabel v2.until)]
                                ]

navPanel : Context -> NavigationSide -> Html Msg
navPanel ctx side =
    let arrowButton = case side of
                          Left -> button ([class "arrowButton", onClick NavigateDiffBack] ++ checkNavBack ctx) [arrow "leftArrow"]
                          Right -> button ([class "arrowButton", onClick NavigateDiffForward] ++ checkNavFwd ctx) [arrow "rightArrow"]
    in div [class "navPanel"] [
           div [class "versionDatesPanel"] [],
           div [class "navPanelItem"] [],
           arrowButton,
           div [class "navPanelItem"] [lockButton ctx side]
        ]

lockButton : Context -> NavigationSide -> Html Msg
lockButton x y = button [class "lockButton"] [lockerIcon Locked "lockedIcon"]

viewVersion : Context -> Maybe Version -> Version -> Html Msg
viewVersion ctx was is =
    let rWas = Maybe.map (Rdap.render ctx.history.identifier << .object) was
        rIs  = Rdap.render ctx.history.identifier is.object
    in  div [ class "version" ]
            [ div [ class "rdap" ] [ Rdap.output <| Rdap.diff rWas rIs ] ]

prettifyDate : Date -> Html a
prettifyDate = text << formatUtc config "%d/%m/%Y %H:%M"

arrow : String -> Html a
arrow svgClass =
    svg [width "50", height "100", viewBox "0 0 50 100", Svg.Attributes.class svgClass]
        [path [strokeWidth "8", fill "transparent" , strokeLinecap "round", strokeLinejoin "round",
                   Svg.Attributes.d "M 10 10 L 40 50 L 10 90"] []]

checkNavBack : Context -> List (Html.Attribute Msg)
checkNavBack ctx = checkNav ctx.history ctx.modificationDate (\h -> h !! ((length h) - 2))

checkNavFwd : Context -> List (Html.Attribute Msg)
checkNavFwd ctx = checkNav ctx.history ctx.modificationDate List.head

checkNav : History -> Date -> (List Version -> Maybe Version) -> List (Html.Attribute Msg)
checkNav history modDate f =
    if Maybe.withDefault False <| Maybe.map ((/=) (Date.toTime modDate)) <| Maybe.map (Date.toTime << .from)
        <| f history.versions
    then []
    else [disabled True]

createDateLabel : Maybe Date -> List (Html a)
createDateLabel md =
    case md of
        Nothing -> [text "Present"]
        Just d  -> [prettifyDate d, moreIcon "moreIconSvg" (toString d)]

moreIcon : String -> String -> Html a
moreIcon svgClass tooltipText =
    svg [viewBox "0 0 100 100", Svg.Attributes.class svgClass] [
        Svg.defs [] [Svg.mask [Svg.Attributes.id "masking"] [
                Svg.rect [Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "100",
                              Svg.Attributes.height "100", Svg.Attributes.fill "white"] [],
                Svg.circle [Svg.Attributes.cx "50", Svg.Attributes.cy "50", Svg.Attributes.r "13.33",
                                Svg.Attributes.fill "black"] [],
                Svg.circle [Svg.Attributes.cx "18.33", Svg.Attributes.cy "50", Svg.Attributes.r "13.33",
                                Svg.Attributes.fill "black"] [],
                Svg.circle [Svg.Attributes.cx "81.66", Svg.Attributes.cy "50", Svg.Attributes.r "13.33",
                                Svg.Attributes.fill "black"] []
                ]
          ],
        Svg.circle [Svg.Attributes.cx "50", Svg.Attributes.cy "50", Svg.Attributes.r "50",
                        Svg.Attributes.mask "url(#masking)"] [
                Svg.title [] [text tooltipText]
            ]
    ]

lockerIcon : LockerState -> String -> Html a
lockerIcon state svgClass =
    let maskPathDraw = case state of
                           Locked   -> "M 35 50 v -20 c 0 -20 30 -20 30 0 v 20"
                           Unlocked -> "M 35 50 v -20 c 0 -20 30 -20 30 0"
    in svg [viewBox "0 0 100 100", Svg.Attributes.class svgClass] [
           Svg.defs [] [Svg.mask [Svg.Attributes.id "lockMask"] [
                   Svg.rect [Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "100",
                                 Svg.Attributes.height "100", fill "white"] [],
                   Svg.rect [Svg.Attributes.x "25", Svg.Attributes.y "45", Svg.Attributes.width "50",
                                 Svg.Attributes.height "30", Svg.Attributes.rx "10", Svg.Attributes.ry "10",
                                 fill "black"] [],
                   Svg.path [Svg.Attributes.d maskPathDraw, strokeWidth "6", strokeLinecap "round",
                                 strokeLinejoin "round", fill "transparent", Svg.Attributes.stroke "black"] []
                   ]
             ],
            Svg.circle [Svg.Attributes.cx "50", Svg.Attributes.cy "50", Svg.Attributes.r "50",
                           Svg.Attributes.mask "url(#lockMask)"] [Svg.title [] [text "Lock version"]]
       ]


type NavigationSide = Left | Right

type LockerState = Locked | Unlocked
