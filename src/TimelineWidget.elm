module TimelineWidget exposing (render, Model)

import Date exposing (Date, year, Month(..), toTime)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Compare exposing (Compare2(..), is)
import Date.Extra.Duration as Duration
import Date.Extra.Field as DEF
import Date.Extra.Format exposing (formatUtc)
import Guards exposing (..)
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, style)
import Html.Events as HE
-- import Svg exposing (svg)
-- import Svg.Attributes exposing (viewBox, fill)
import List exposing (range, head)
import List.Extra exposing ((!!), last)
import Maybe exposing (withDefault, map)
import Maybe.Extra exposing (join)
import Svg exposing (svg, path)
import Svg.Attributes as SA
import Svg.Events as SE

import Model exposing (Msg(..), Version, TimelineZoom(..))

import Debug exposing (log)

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
    in div [class "timelineWidget"] boxes

box : Model -> Date -> Html Msg
box m d =
    let measurements = calculateMeasurements m d
    in div [class "timelineBox"] <|
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
                    Month    -> formatUtc config "%B %Y" d
    in [div [class "timelineBoxTitle"] [text title]]

timelineLifeline : List BoxVersionMeasurement -> List (Html a)
timelineLifeline =
    let createDiv (x, y, a)= div [class <| divClass a,
                                    style [("left", (toString x) ++ "%"), ("width", (toString <| y - x) ++ "%")]] []
        divClass a = "timelineLifeline" ++ if a then " active" else ""
    in List.map (\vm -> createDiv (max 0 vm.fromPct, min 100 vm.untilPct, vm.active))

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
        createDiv x = div [class (divClass x), style [("left", (toString x) ++ "%")]] []
    in List.map createDiv scales

zoomBox : Model -> Date -> List (Html Msg)
zoomBox m d =
    let pct x = toFloat x * (100/12)
    in case m.zoom of
           Lifetime -> [div [class "zoomBoxYear", HE.onClick (ZoomTimelineWidget Year (Just d))] [], zoomIcon "zoomIcon"]
           Year     -> List.map (\x -> div [class "zoomBoxMonth", style [("left", (toString <| pct (x - 1)) ++ "%"),
                                                                         ("width", (toString <| pct x - pct (x - 1)) ++ "%")],
                                            HE.onClick (ZoomTimelineWidget Month
                                                            (Just <| Duration.add Duration.Month (x - 1) d))]
                                     [zoomIcon "zoomIcon"])
                                (List.range 1 12)
           Month    -> []

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
        Year     -> [button [class "zoomBackButton", HE.onClick <| ZoomTimelineWidget Lifetime Nothing] [zoomBackIcon]]
        Month    -> [button [class "zoomBackButton", HE.onClick <| ZoomTimelineWidget Year <|
                                 Maybe.map (DEF.fieldToDateClamp (DEF.Month Jan)) m.zoomDate] [zoomBackIcon]]

markerButton : Bool -> Maybe Msg -> String -> String -> Html Msg
markerButton selected mMsg style tooltip =
    let svgClass = if selected == True then " selected" else ""
        action = case mMsg of
                     Nothing -> []
                     Just msg -> [SE.onClick msg]
    in svg [SA.viewBox "0 0 20 50", SA.class ("timelineMarker " ++ svgClass), SA.style style]
           [
              Svg.line ([SA.x1 "10", SA.y1 "20", SA.x2 "10", SA.y2 "50"] ++ action) [Svg.title [] [Svg.text tooltip]],
              Svg.circle ([SA.cx "10", SA.cy "10", SA.r "7", SA.style "stroke-width: 6"] ++ action) [Svg.title [] [Svg.text tooltip]]
           ]

zoomIcon : String -> Html a
zoomIcon svgClass =
    Svg.svg [SA.viewBox "0 0 24 24", SA.class svgClass]
        [Svg.path [SA.strokeLinecap "round", SA.strokeLinejoin "round",
                   SA.d "M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"] []]

zoomBackIcon : Html a
zoomBackIcon =
    let svgClass = "zoomBackIcon"
    in Svg.svg [SA.viewBox "0 0 1400 1400", SA.class svgClass] [
        Svg.defs [] [Svg.mask [SA.id svgClass] [
                Svg.rect [SA.x "0", SA.y "0", SA.width "1400", SA.height "1400", SA.fill "white"] [],
                path [SA.strokeWidth "17", SA.fill "black" , SA.strokeLinecap "round", SA.strokeLinejoin "round",
                          SA.d "M 500,304.6 V 147.8 c 0,-15.7 -6,-31.4 -17.9,-43.4 -11.9,-11.9 -27.6,-17.9 -43.3,-17.9 -15.7,0 -31.4,5.9 -43.3,17.9 L 10,484.7 395.4,864.9 c 11.9,11.9 27.6,17.9 43.3,17.9 15.7,0 31.4,-6 43.3,-17.9 11.9,-11.9 17.9,-27.7 17.9,-43.3 V 669.1 c 168.4,4.2 352.5,34.7 490,244.3 V 852.1 C 990,568.4 775.6,335.1 500,304.6 Z",
                          SA.stroke "black", SA.transform "translate(200, 150)"] []
              ]
          ],
        Svg.circle [SA.cx "700", SA.cy "700", SA.r "700", SA.mask <| "url(#" ++ svgClass ++ ")"] [Svg.title [] [Svg.text "Zoom back"]]
    ]
