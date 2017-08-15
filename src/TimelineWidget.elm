module TimelineWidget exposing (render, Model, Zoom(..))

import Date exposing (Date, year, Month(..), toTime)
import Date.Extra.Create exposing (dateFromFields)
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

import Model exposing (Msg, Version)

import Debug exposing (log)

type alias Model =
    {
        zoom : Zoom,
        target : Maybe Date,
        versions : List Version,
        today: Date
    }

type Zoom
    = Lifetime
    | Year
    | Month

render : Model -> Html Msg
render m =
    let boxes = case m.zoom of
                    Lifetime -> List.map (\y -> box m <| dateFromFields y Jan 1 0 0 0 0) <| years m.versions
                    Year     -> []
                    Month    -> []
        years : List Version -> List Int
        years vs = range (withDefault 0 <| map (year << .from) <| last vs)
                         (withDefault (year m.today) <| map year <| join <| map .until <| head vs)
    in div [class "timelineWidget"] boxes

box : Model -> Date -> Html Msg
box m d =
    div [class "timelineBox"] <|
        timelineBoxTitle m d ++
        timelineLifeline m d ++
        [
            markerIcon False Nothing ""
        ]

type alias BoxVersionMeasurement =
    {
        fromPct: Float,
        untilPct: Float,
        from: Date,
        selected: Bool
    }

timelineBoxTitle : Model -> Date -> List (Html a)
timelineBoxTitle m d =
    let title = case m.zoom of
                    Lifetime -> toString <| year d
                    Year     -> toString <| year d
                    Month    -> ""
    in [div [class "timelineBoxTitle"] [text title]]


timelineLifeline : Model -> Date -> List (Html a)
timelineLifeline m d =
    let createDiv (x, y) = div [class "timelineLifeline",
                                    style [("left", (toString x) ++ "%"), ("width", (toString y) ++ "%")]] []
    in List.map createDiv <| prune pcts

timelinePercentages : List Version -> Date -> Date -> Date -> List (Float, Float)
timelinePercentages vs startDate endDate today =
--    let total = (toTime endDate) - (toTime startDate)
    let total = (toTime (log "endDate" endDate)) - (toTime (log "startDate" startDate))
        convert v = (convertDate v.from, convertDate <| withDefault today v.until)
        convertDate d = ((toTime d) - (toTime startDate)) / total * 100
--    in List.map convert vs
    in log "result" <| List.map convert (log "vs" vs)


timelineMarkers : Model -> Date -> List (Html Msg)
timelineMarkers m d =
    let pcts = timelinePercentages m.versions d (Duration.add period 1 d) m.today
        period = case m.zoom of
                    Lifetime -> Duration.Year
                    Year     -> Duration.Year
                    Month    -> Duration.Month
        prune versions = case versions of
                       [] -> []
                       ((x, y) :: xs) -> if ((x < 0) && (y < 0)) || ((x > 100) && (y > 100))
                                         then prune xs
                                         else (max 0 x, min 100 y) :: prune xs
        createDiv (x, y) = div [class "timelineLifeline",
                                    style [("left", (toString x) ++ "%"), ("width", (toString y) ++ "%")]] []
    in List.map createDiv <| prune pcts

markerIcon : Bool -> Maybe Msg -> String -> Html Msg
markerIcon selected mMsg style =
    let svgClass = if selected == True then " selected" else ""
        action = case mMsg of
                     Nothing -> []
                     Just msg -> [SE.onClick msg]
    in svg [SA.viewBox "0 0 20 50", SA.class ("timelineMarker " ++ svgClass), SA.style style]
           [
              Svg.line ([SA.x1 "10", SA.y1 "20", SA.x2 "10", SA.y2 "50"] ++ action) [],
              Svg.circle ([SA.cx "10", SA.cy "10", SA.r "10"] ++ action) []
           ]
