module Timeline exposing (timeline)

import Date exposing (Date, toTime, fromString)
import Html exposing (Html)
import Maybe
import Result exposing (withDefault)
import Svg exposing (svg, rect)
import Svg.Attributes as A exposing (width, height, x, y, class)
import VirtualDom
import Model exposing (..)


-- asTime   s = Date.toTime <| Result.withDefault now <| Date.fromString s
-- timespan v = { whence = asTime v.applicability.whence
--              , until  = asTime v.applicability.until }
-- times = List.map timespan h.versions
-- minmax = List.concatMap (\p -> [p.whence, p.until]) times
-- earliest = List.minimum minmax
-- latest   = List.maximum minmax
-- minimax : List (Int, Int) -> (Int, Int)
-- minimax vs =
--     in
--         mm (0, 1) vs


minimax : List ( Float, Float ) -> ( Float, Float )
minimax vs =
    let
        flat =
            List.concatMap (\( a, b ) -> [ a, b ]) vs
    in
        ( Maybe.withDefault 0 (List.minimum flat)
        , Maybe.withDefault 1 (List.maximum flat)
        )

timeline : Date -> List Version -> Html msg
timeline now ps =
    let asResult s   = Result.andThen fromString <| Result.fromMaybe "" s
        asTime s     = toTime <| withDefault now <| asResult s
        timespan p   = ( asTime (Maybe.Just p.from), asTime p.until )
        times        = List.map timespan ps
        ( min, max ) = minimax times
        span         = max - min
        scale v      = toString <| (v - min) * 100 / span
        existed ( f, t ) =
            rect
                [ x (scale f)
                , y "0"
                , width (scale (min + t - f))
                , height "50"
                , class "existed"
                , vectorEffect "non-scaling-stroke"
                ]
                []
    in
        svg
            -- Attributes
            [ A.viewBox "0 0 100 50"
            , A.preserveAspectRatio "none"
            , A.class "timeline"
            , vectorEffect "non-scaling-stroke"
            ]
        <|
            -- Children
            Svg.text (toString ( min, max ))
                :: rect [ x "0", y "0", width "100", height "50", class "empty" ] []
                :: List.map existed times


vectorEffect : String -> Svg.Attribute msg
vectorEffect =
    VirtualDom.attribute "vector-effect"
