module TimelineWidget exposing (render, Model, Zoom(..))

import Date exposing (Date, year, Month(..), toTime)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Compare exposing (Compare2(..), is)
import Date.Extra.Duration as Duration
import Guards exposing (..)
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, style)
-- import Svg exposing (svg)
-- import Svg.Attributes exposing (viewBox, fill)
import List exposing (range, head)
import List.Extra exposing ((!!), last)
import Maybe exposing (withDefault, map)
import Maybe.Extra exposing (join)
import Svg exposing (svg, path)
import Svg.Attributes as SA
import Svg.Events as SE

import Model exposing (Msg(..), Version)

import Debug exposing (log)

type alias Model =
    {
        zoom : Zoom,
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

type Zoom
    = Lifetime
    | Year
    | Month

render : Model -> Html Msg
render m =
    let boxes = case m.zoom of
                    Lifetime -> List.map (\y -> box m <| dateFromFields y Jan 1 0 0 0 0) <| years m.versions
                    Year    -> []
                    Month    -> []
        years : List Version -> List Int
        years vs = range (withDefault 0 <| map (year << .from) <| last vs)
                         (withDefault (year m.today) <| map year <| join <| map .until <| head vs)
    in div [class "timelineWidget"] boxes

box : Model -> Date -> Html Msg
box m d =
    let measurements = calculateMeasurements m d
    in div [class "timelineBox"] <|
           timelineBoxTitle m d ++
           timelineLifeline measurements ++
           timelineScales m.zoom d ++
           timelineMarkers m measurements

timelineBoxTitle : Model -> Date -> List (Html a)
timelineBoxTitle m d =
    let title = case m.zoom of
                    Lifetime -> toString <| year d
                    Year     -> toString <| year d
                    Month    -> ""
    in [div [class "timelineBoxTitle"] [text title]]

timelineLifeline : List BoxVersionMeasurement -> List (Html a)
timelineLifeline =
    let createDiv (x, y, a)= div [class <| divClass a,
                                    style [("left", (toString x) ++ "%"), ("width", (toString <| y - x) ++ "%")]] []
        divClass a = "timelineLifeline" ++ if a then " active" else ""
    in List.map (\vm -> createDiv (max 0 vm.fromPct, min 100 vm.untilPct, vm.active))

timelineScales : Zoom -> Date -> List (Html a)
timelineScales z d =
    let yearlyScale = List.map (\x -> toFloat x * (100/12)) (List.range 0 12)
        scales = case z of
                     Lifetime -> yearlyScale
                     Year     -> yearlyScale
                     Month    -> List.map (\x -> toFloat x * (100/(toFloat numDays))) (List.range 0 numDays)
        numDays = Duration.diffDays d (Duration.add Duration.Month 1 d)
        divClass x = List.member x [0.0, 100.0] => "timelineScaleFull"
                  |= x == 50.0                  => "timelineScaleHalf"
                  |= "timelineScale"
        createDiv x = div [class (divClass x), style [("left", (toString x) ++ "%")]] []
    in List.map createDiv scales

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
