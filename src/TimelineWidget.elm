module TimelineWidget exposing (render, Model)

import Date exposing (Date, year, Month(..), toTime)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Compare exposing (Compare2(..), is)
import Date.Extra.Duration as Duration
import Date.Extra.Field as DEF
import Date.Extra.Format exposing (format)
import Guards exposing (..)
import Html exposing (Html, div, text, button)
import Html.Attributes as HA
import Html.Events as HE
import List exposing (range, head)
import List.Extra exposing ((!!), last)
import Maybe exposing (withDefault, map)
import Maybe.Extra exposing (join)

import Icons exposing (..)
import Model exposing (Msg(..), Version, TimelineZoom(..), NavigationDirection(..))

type alias Model =
    {
        zoom : TimelineZoom,
        zoomDate : Maybe Date,
        displayedVersions : (Maybe Version, Maybe Version),
        versions : List Version,
        today: Date
    }

type alias BoxVersionMeasurement =
    {
        fromPct: Float,
        untilPct: Float,
        version: Version,
        selected: Bool,
        active: Bool
    }

render : Model -> Html Msg
render m =
    let boxes = case m.zoom of
                    Lifetime -> List.map (\y -> box m <| dateFromFields y Jan 1 0 0 0 0) <| years m.versions
                    Year     -> zoomedBox
                    Month    -> zoomedBox
        years : List Version -> List Int
        years vs = range (withDefault 0 <| map (year << .from) <| last vs)
                         (withDefault (year m.today) <| map year <| join <| map .until <| head vs)
        zoomedBox = case m.zoomDate of
                      Nothing -> []
                      Just d  -> [box m d]
        createNavPanel dir = if m.zoom == Lifetime then [] else [navPanel m dir]
    in div [HA.class "timelineWidget"] (createNavPanel Bkwd ++ boxes ++ createNavPanel Fwd)

box : Model -> Date -> Html Msg
box m d =
    let measurements = calculateMeasurements m d
    in div [HA.class "timelineBox"] <|
           timelineBoxTitle m d ++
           timelineLifeline measurements ++
           timelineScales m.zoom d ++
           zoomBox m d ++
           timelineMarkers m measurements ++
           zoomBack m

timelineBoxTitle : Model -> Date -> List (Html a)
timelineBoxTitle m d =
    let title = case m.zoom of
                    Lifetime -> toString <| year d
                    Year     -> toString <| year d
                    Month    -> format config "%B %Y" d
    in [div [HA.class "timelineBoxTitle"] [text title]]

timelineLifeline : List BoxVersionMeasurement -> List (Html a)
timelineLifeline =
    let createDiv (x, y, a)= div [HA.class <| divClass a,
                                    HA.style [("left", (toString x) ++ "%"), ("width", (toString <| y - x) ++ "%")]] []
        divClass a = "timelineLifeline" ++ if a then " active" else ""
    in (++) [div [HA.class "timelineLifelineGuide"][]] <<
        List.map (\vm -> createDiv (max 0 vm.fromPct, min 100 vm.untilPct, vm.active))

timelineScales : TimelineZoom -> Date -> List (Html a)
timelineScales z d =
    let yearlyScale = List.map (\x -> toFloat x * (100/12)) (List.range 0 12)
        scales = case z of
                     Lifetime -> yearlyScale
                     Year     -> yearlyScale
                     Month    -> List.map (\x -> toFloat x * (100/(toFloat numDays))) (List.range 0 numDays)
        numDays = Duration.diffDays (Duration.add Duration.Month 1 d) d
        divClass x = List.member x [0.0, 100.0] => "timelineScaleFull"
                  |= x == 50.0                  => "timelineScaleHalf"
                  |= "timelineScale"
        createDiv x = div [HA.class (divClass x), HA.style [("left", (toString x) ++ "%")]] []
    in List.map createDiv scales

zoomBox : Model -> Date -> List (Html Msg)
zoomBox m d =
    let pct x = toFloat x * (100/12)
    in case m.zoom of
           Lifetime -> [div [HA.class "zoomBoxYear", HE.onClick (ZoomTimelineWidget Year (Just d))] []]
           Year     -> List.map (\x -> div [HA.class "zoomBoxMonth", HA.style [("left", (toString <| pct (x - 1)) ++ "%"),
                                                                         ("width", (toString <| pct x - pct (x - 1)) ++ "%")],
                                            HE.onClick (ZoomTimelineWidget Month
                                                            (Just <| Duration.add Duration.Month (x - 1) d))]
                                     [])
                                (List.range 1 12)
           Month    -> []

navPanel : Model -> NavigationDirection -> Html Msg
navPanel model direction =
    let arrowButton = case direction of
                          Bkwd -> button [HA.class "arrowButton left", HE.onClick action,
                                              HA.disabled <| checkNavDisabled model direction] [arrow "timelineWidgetLeftArrow"]
                          Fwd -> button [HA.class "arrowButton right", HE.onClick action,
                                              HA.disabled <| checkNavDisabled model direction] [arrow "timelineWidgetRightArrow"]
        action = ZoomTimelineWidget model.zoom
                 (Maybe.map3 Duration.add (Just duration) (Just (if direction == Fwd then 1 else -1)) model.zoomDate)
        duration = if model.zoom == Month then Duration.Month else Duration.Year
    in div [HA.class "timelineWidgetNavPanel"] [arrowButton]

checkNavDisabled : Model -> NavigationDirection -> Bool
checkNavDisabled m dir =
    let isLastYear = (Maybe.map Date.year m.zoomDate) ==
                     (Maybe.map (Date.year << Maybe.withDefault m.today << .until) <| List.head m.versions)
        isLastMonth = isLastYear && ((Maybe.map Date.month m.zoomDate) ==
                                         (Maybe.map (Date.month << Maybe.withDefault m.today << .until) <| List.head m.versions))
        isFistYear = (Maybe.map Date.year m.zoomDate) == (Maybe.map (Date.year << .from) <| List.Extra.last m.versions)
        isFistMonth = isFistYear && ((Maybe.map Date.month m.zoomDate) ==
                                         (Maybe.map (Date.month << .from) <| List.Extra.last m.versions))
    in case m.zoom of
        Lifetime -> True
        Year     -> if dir == Fwd then isLastYear else isFistYear
        Month    -> if dir == Fwd then isLastMonth else isFistMonth

calculateMeasurements : Model -> Date -> List BoxVersionMeasurement
calculateMeasurements m startDate =
    let endDate = Duration.add period 1 startDate
        period = case m.zoom of
                    Lifetime -> Duration.Year
                    Year     -> Duration.Year
                    Month    -> Duration.Month
        total = (toTime endDate) - (toTime startDate)
        convert v = BoxVersionMeasurement (convertDate v.from)
                                          (convertDate <| withDefault m.today v.until)
                                          v
                                          (isSelected v)
                                          (isSelected v)
        convertDate d = ((toTime d) - (toTime startDate)) / total * 100
        isSelected v = case m.displayedVersions of
                           (Nothing, Nothing) -> False
                           (Just v2, Just v1) -> (is After v.from v2.from) &&
                                                 (is SameOrBefore v.from v1.from)
                           _                  -> True
        -- we need to activate previous selected ones for the lifeline
        activateReverse = activateNext << List.reverse
        activateNext vms = case vms of
                               vm1 :: vm2 :: r -> {vm1 | active = vm1.selected || vm2.selected} ::
                                                      activateNext (vm2 :: r)
                               vm :: []        -> [vm]
                               []              -> []
        prune = List.filter (\p -> not (((p.fromPct < 0) && (p.untilPct < 0)) ||
                                       ((p.fromPct > 100) && (p.untilPct > 100))))
    in prune <| activateReverse <| List.map convert m.versions

timelineMarkers : Model -> List BoxVersionMeasurement -> List (Html Msg)
timelineMarkers model =
    let style meas = "left: " ++ (toString meas.fromPct) ++ "%"
        createMarker meas = markerButton meas.selected (Just <| NavigateDiffToVersion meas.version) (style meas)
                                (toString meas.version.from)
        prune = List.filter (\meas -> meas.fromPct >= 0 && Just meas.version /= List.Extra.last model.versions)
    in List.map createMarker << prune

zoomBack : Model -> List (Html Msg)
zoomBack m =
    case m.zoom of
        Lifetime -> []
        Year     -> [button [HA.class "zoomOutButton", HE.onClick <| ZoomTimelineWidget Lifetime Nothing, HA.title "Zoom out"] [zoomOutIcon]]
        Month    -> [button [HA.class "zoomOutButton", HE.onClick <| ZoomTimelineWidget Year <|
                                 Maybe.map (DEF.fieldToDateClamp (DEF.Month Jan)) m.zoomDate] [zoomOutIcon]]
