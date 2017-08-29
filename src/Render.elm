module Render exposing (viewAsList, Context)

import Date exposing (Date)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Compare exposing (Compare2(..), is)
import Date.Extra.Format exposing (formatUtc, isoDateFormat)
import Html exposing (div, Html, ol, li, span, text, button, select, option)
import Html.Attributes exposing (class, value, id, title, disabled, style, src)
import Html.Events exposing (onWithOptions, onInput, onClick)
import List exposing (length, head)
import List.Extra exposing ((!!), dropWhile, last)
import Maybe exposing (map, map2, map3, withDefault)
import Maybe.Extra exposing (isJust, (?), join)
import Tuple exposing (..)

import Icons exposing (..)
import Model exposing (..)
import Rdap
import TimelineWidget

-- Rendering happens inside a context
type alias Context = {
    history : History,
    fromVersion : Maybe Version,
    toVersion : Maybe Version,
    versions : List Version, -- TODO: rename this since it can be mistakely taken as all versions
    navigationLocks : (LockerState, LockerState),
    versionDateDetail : Maybe Date,
    today: Date,
    timelineWidgetZoom : TimelineZoom,
    timelineWidgetZoomDate : Maybe Date
}

type DateFormat = Short | Long

mkCtx : Date -> History -> Model -> Context
mkCtx today h m =
    let (fromVersion, toVersion) = m.displayedVersions
        versions = Maybe.Extra.values [fromVersion, toVersion]
    in Context h fromVersion toVersion versions m.navigationLocks m.versionDateDetail today m.timelineWidgetZoom m.timelineWidgetZoomDate

viewAsList : Response -> Model -> List (Html Msg)
viewAsList response m =
    let history = response.history !! m.selected
    in case history of
           Nothing -> [] -- TODO: will never happen. deal with it differently?
           Just h -> let ctx = mkCtx response.stamp h m
                     in [div [class "historyPane"] ((objectListPanel response.history m.selected) ++
                                                        (objectListMobile response.history m.selected) ++
                                                        (detailPanel ctx))]

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

objectListMobile : List History -> Int -> List (Html Msg)
objectListMobile historyList idx =
    if length historyList <= 1
    then []
    else [div[class "objectListMobile"] [text "Search results: ", dropdownResultsMobile historyList idx]]

dropdownResultsMobile : List History -> Int -> Html Msg
dropdownResultsMobile historyList idx =
    let clickedIndex s = Result.withDefault 0 (String.toInt s)
    in select [Html.Events.onInput (\s -> Select <| clickedIndex s)] <| List.indexedMap (\x h -> option [Html.Attributes.value <| toString x] [text h.identifier.handle]) historyList

-- Detail Panel

detailPanel : Context -> List (Html Msg)
detailPanel ctx =
    [ div [class "detailPanel"] [
        timelineWidgetPanel ctx,
        div [class "detailPanelMain"] [
            navPanel ctx Bkwd,
            div [class "detailCenterPanel"] ([versionDatesPanel ctx] ++ versionDateDetailPanels ctx ++ [diffPanel ctx]),
            navPanel ctx Fwd,
            navBottomPanel ctx
        ]
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
                                div [class "versionDateLeft"] [span [] ((createDateLabel (Just v.from) ctx.versionDateDetail) ++ [text ">"])],
                                div [class "versionDateRight"] [span []([text "<"] ++ createDateLabel v.until ctx.versionDateDetail)]
                            ]
        v1 :: v2 :: _ ->
            let versionsInBetween = (+) (-1) <| withDefault 0 <| getDistance v1 v2 ctx.history.versions
                middleLabel = if versionsInBetween == 0
                              then createDateLabel (Just v2.from) ctx.versionDateDetail
                              else [text <| (toString versionsInBetween) ++
                                        " version" ++ (if versionsInBetween > 1 then "s" else "")]
            in div [class "versionDatesPanel"] [
                                   div [class "versionDateLeft"]
                                       [span [] ((createDateLabel (Just v1.from) ctx.versionDateDetail) ++ [text ">"])],
                                   div [class "versionDateCenter"] [span [] ([text "<"] ++ middleLabel ++ [text ">"])],
                                   div [class "versionDateRight"] [span [] ([text "<"] ++ createDateLabel v2.until ctx.versionDateDetail)]
                                ]

versionDateDetailPanels : Context -> List (Html a)
versionDateDetailPanels ctx =
    let createDiv pos (panelClass, dateString) = div [class <| "versionDateDetailPanel " ++ pos ++ " " ++ panelClass]
                                                     [text dateString]
        process date = if date /= Nothing && ctx.versionDateDetail == date
                       then ("showPanel", Maybe.withDefault "" <| Maybe.map toString date)
                       else ("hidePanel", "")
    in [createDiv "left" <| process <| Maybe.map .from ctx.fromVersion,
        createDiv "center" <| process <| Maybe.map .from ctx.toVersion,
        createDiv "right" <| process <| Maybe.Extra.join <| Maybe.map .until ctx.toVersion]

timelineWidgetPanel : Context -> Html Msg
timelineWidgetPanel ctx =
    TimelineWidget.render <| mkTimelineModel ctx

navPanel : Context -> NavigationDirection -> Html Msg
navPanel ctx direction =
    div [class "navPanel"] [
        div [class "versionDatesPanel"] [],
        div [class "navPanelButtons"] [arrowButton ctx direction,
                                        lockButton ctx direction <| "navPanel" ++ toString direction]
     ]

navBottomPanel : Context -> Html Msg
navBottomPanel ctx =
    div [class "navBottomPanel"]
        [
            div [] [lockButton ctx Bkwd "navBottomBkwd", arrowButton ctx Bkwd],
            div [] [arrowButton ctx Fwd, lockButton ctx Fwd "navBottomFwd"]
        ]

arrowButton : Context -> NavigationDirection -> Html Msg
arrowButton ctx direction =
    case direction of
        Bkwd -> button [class "arrowButton", onClick (NavigateDiff Bkwd),
                            disabled <| checkNavDisabled ctx direction] [arrow "leftArrow"]
        Fwd -> button [class "arrowButton", onClick (NavigateDiff Fwd),
                            disabled <| checkNavDisabled ctx direction] [arrow "rightArrow"]

lockButton : Context -> NavigationDirection -> String -> Html Msg
lockButton ctx dir id =
    let f = if dir == Fwd then second else first
        state = f ctx.navigationLocks
        buttonClass = if state == Locked then "lockedButton" else "unlockedButton"
    in button [class buttonClass, onClick (FlipNavLock dir)] [lockerIcon state "lockerIcon" id]

viewDiff : Context -> Maybe Version -> Version -> List (Html Msg)
viewDiff ctx was is =
    let rWas = map (Rdap.render ctx.history.identifier << .object) was
        rIs  = Rdap.render ctx.history.identifier is.object
        diff = Rdap.diff rWas rIs
        diffOutput = Rdap.output diff
        mobileDiffOutput = Rdap.mobileOutput diff
    in case was of
           Nothing -> [ div [class "diffPanelItem"] [div [class "rdap-is"] [diffOutput] ],
                             div [class "rdap-mobile"] [mobileDiffOutput]]
           _       -> [ div [class "diffPanelItem rdap-was"] [diffOutput],
                        div [class "diffPanelItem rdap-is"] [diffOutput],
                        div [class "rdap-mobile"] [mobileDiffOutput]
                      ]

prettifyDate : Date -> DateFormat -> Bool -> Html a
prettifyDate d df active =
    let pattern = case df of
                      Short -> "%d/%m/%y"
                      Long  -> "%d/%m/%Y %H:%M"
        spanClass = case df of
                        Short -> "dateShort"
                        Long  -> "dateLong"
        activeClass = if active then " active" else ""
    in span [class <| spanClass ++ activeClass] [text <| formatUtc config pattern d]

createDateLabel : Maybe Date -> Maybe Date -> List (Html Msg)
createDateLabel md versionDateDetail =
    case md of
        Nothing -> [text "Present"]
        Just d  -> let flipTo = case versionDateDetail of
                                    Nothing -> md
                                    Just vd -> if (is Same d vd) then Nothing else md
                       (buttonClass, tooltipText, active) = if flipTo == Nothing
                                                    then ("pressedFlipShowVersionButton", "Hide date detail", True)
                                                    else ("flipShowVersionButton", "Show date detail", False)
                   in [ prettifyDate d Short active, prettifyDate d Long active,
                         button [class buttonClass, onClick (FlipShowVersionDateDetail flipTo), title tooltipText]
                                [expandIcon "moreIconSvg"]
                      ]


-- Utility methods

checkNavDisabled : Context -> NavigationDirection -> Bool
checkNavDisabled ctx dir =
    not <| Model.canNavigate ctx.history.versions (ctx.fromVersion, ctx.toVersion) dir ctx.navigationLocks

mkTimelineModel : Context -> TimelineWidget.Model
mkTimelineModel ctx =
    TimelineWidget.Model ctx.timelineWidgetZoom ctx.timelineWidgetZoomDate (ctx.fromVersion, ctx.toVersion)
        ctx.history.versions ctx.today
