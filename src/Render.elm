module Render exposing (viewAsList)

import Date exposing (Date)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format exposing (formatUtc, isoDateFormat)
import Html exposing (div, Html, ol, li, span, text, button)
import Html.Attributes exposing (class, value, id, title, disabled, style, src)
import Html.Events exposing (onWithOptions, onInput, onClick)
import List exposing (length, head)
import List.Extra exposing ((!!), dropWhile, last)
import Maybe exposing (map, map2, map3, withDefault)
import Maybe.Extra exposing (isJust, (?), join)
import Svg exposing (svg, path)
import Svg.Attributes exposing (width, height, viewBox, strokeLinecap, strokeLinejoin, strokeWidth, fill)
import Tuple exposing (..)

import Model exposing (..)
import Rdap

-- Rendering happens inside a context
type alias Context = {
    history : History,
    fromVersion : Maybe Version,
    toVersion : Maybe Version,
    versions : List Version, -- TODO: rename this since it can be mistakely taken as all versions
    navigationLocks : (LockerState, LockerState)
}

mkCtx : History -> (Maybe Version, Maybe Version) -> (LockerState, LockerState) -> Context
mkCtx h (fromVersion, toVersion) navigationLockers =
    let versions = Maybe.Extra.values [fromVersion, toVersion]
    in Context h fromVersion toVersion versions navigationLockers

viewAsList : Response -> Int -> (Maybe Version, Maybe Version) -> (LockerState, LockerState) -> List (Html Msg)
viewAsList response idx displayedVersions navigationLocks =
    let history = response.history !! idx
    in case history of
           Nothing -> [] -- TODO: will never happen. deal with it differently?
           Just h -> let ctx = mkCtx h displayedVersions navigationLocks
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
        navPanel ctx Bkwd,
        div [class "detailCenterPanel"] [
          versionDatesPanel ctx,
          diffPanel ctx
        ],
        navPanel ctx Fwd
    ]]

diffPanel : Context -> Html Msg
diffPanel ctx =
    case ctx.toVersion of
        Nothing -> text "Nothing to show"
        Just v  -> div [class "diffPanel"] (viewDiff ctx ctx.fromVersion v)

versionDatesPanel : Context -> Html Msg
versionDatesPanel ctx =
    case ctx.versions of
        []            -> text ""
        v :: []       -> div [class "versionDatesPanel"] [
                                div [class "versionDateLeft"] [span [] ((createDateLabel (Just v.from)) ++ [text " >"])],
                                div [class "versionDateRight"] [span []([text "< "] ++ createDateLabel v.until)]
                            ]
        v1 :: v2 :: _ ->
            let versionsInBetween = (+) (-1) <| withDefault 0 <| getDistance v1 v2 ctx.history.versions
                middleLabel = if versionsInBetween == 0
                              then createDateLabel (Just v2.from)
                              else [text <| (toString versionsInBetween) ++
                                        " version" ++ (if versionsInBetween > 1 then "s" else "")]
            in div [class "versionDatesPanel"] [
                                   div [class "versionDateLeft"]
                                       [span [] ((createDateLabel (Just v1.from)) ++ [text " >"])],
                                   div [class "versionDateCenter"] [span [] ([text "< "] ++ middleLabel ++ [text " >"])],
                                   div [class "versionDateRight"] [span [] ([text "< "] ++ createDateLabel v2.until)]
                                ]

navPanel : Context -> NavigationDirection -> Html Msg
navPanel ctx direction =
    let arrowButton =
            case direction of
                Bkwd -> button [class "arrowButton", onClick (NavigateDiff Bkwd),
                                    disabled <| checkNavDisabled ctx direction] [arrow "leftArrow"]
                Fwd -> button [class "arrowButton", onClick (NavigateDiff Fwd),
                                    disabled <| checkNavDisabled ctx direction] [arrow "rightArrow"]
    in div [class "navPanel"] [
           div [class "versionDatesPanel"] [],
           div [class "navPanelItem"] [],
           arrowButton,
           div [class "navPanelItem"] [lockButton ctx direction]
        ]

lockButton : Context -> NavigationDirection -> Html Msg
lockButton ctx dir =
    let f = if dir == Fwd then second else first
        state = f ctx.navigationLocks
        buttonClass = if state == Locked then "lockedButton" else "unlockedButton"
    in button [class buttonClass, onClick (FlipNavLock dir)] [lockerIcon state <| "lockedIcon" ++ (toString dir)]

viewDiff : Context -> Maybe Version -> Version -> List (Html Msg)
viewDiff ctx was is =
    let rWas = map (Rdap.render ctx.history.identifier << .object) was
        rIs  = Rdap.render ctx.history.identifier is.object
        diffOutput = Rdap.output <| Rdap.diff rWas rIs
    in case was of
           Nothing -> [ div [class "diffPanelItem"] [ div [class "rdap-is"] [diffOutput] ] ]
           _       -> [ --div [class "diffPanelItem rdap-was"] [diffOutput],
                        div [class "diffPanelItem rdap-is"] [diffOutput]
                      ]

prettifyDate : Date -> Html a
prettifyDate = text << formatUtc config "%d/%m/%y"

arrow : String -> Html a
arrow svgClass =
    svg [width "50", height "100", viewBox "0 0 50 100", Svg.Attributes.class svgClass]
        [path [strokeWidth "8", fill "transparent" , strokeLinecap "round", strokeLinejoin "round",
                   Svg.Attributes.d "M 10 10 L 40 50 L 10 90"] []]

createDateLabel : Maybe Date -> List (Html a)
createDateLabel md =
    case md of
        Nothing -> [text "Present"]
        Just d  -> [prettifyDate d, moreIcon "moreIconSvg" (toString d)]


 -- Icons

moreIcon : String -> String -> Html a
moreIcon svgClass tooltipText =
    svg [viewBox "0 0 100 100", Svg.Attributes.class svgClass] [
        Svg.defs [] [Svg.mask [Svg.Attributes.id svgClass] [
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
                        Svg.Attributes.mask <| "url(#" ++ svgClass ++ ")"] [
                Svg.title [] [text tooltipText]
            ]
    ]

-- Note: We need to change the mask name otherwise Chrome won't update the icon. Only updating the mask's path
--       is not enough.
lockerIcon : LockerState -> String -> Html a
lockerIcon state svgClass =
    let maskPathDraw = case state of
                           Locked   -> "M 35 50 v -20 c 0 -20 30 -20 30 0 v 20"
                           Unlocked -> "M 35 50 v -20 c 0 -20 30 -20 30 0"
        maskName = toString state ++ svgClass
        iconTitle = if state == Locked then "Unlock version" else "Lock version"
    in svg [viewBox "0 0 100 100", Svg.Attributes.class svgClass] [
           Svg.defs [] [Svg.mask [Svg.Attributes.id maskName] [
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
                           Svg.Attributes.mask <| "url(#" ++ maskName ++ ")"] [Svg.title [] [text iconTitle]]
       ]


 -- Utility methods

checkNavDisabled : Context -> NavigationDirection -> Bool
checkNavDisabled ctx dir =
    not <| Model.canNavigate ctx.history.versions (ctx.fromVersion, ctx.toVersion) dir ctx.navigationLocks
