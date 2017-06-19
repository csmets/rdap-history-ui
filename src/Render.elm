module Render exposing (viewAsList)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class, value, id, title)
import Html.Events exposing (onWithOptions, onInput, onClick)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format exposing (formatUtc, isoDateFormat)
import Svg exposing (svg, path)
import Svg.Attributes exposing (width, height, viewBox, strokeLinecap, strokeLinejoin, strokeWidth, fill)

import Model exposing (..)
import Rdap
import Util exposing ((!!))

-- TODO figure out if needed, and move to common module if so
type alias Response =
    { stamp : Date.Date
    , history : List History
    }

-- Rendering happens inside a context
type alias Context =
    { today : Date
    , identifier : Identifier
    }

mkCtx : Date -> History -> Context
mkCtx today history = Context today history.identifier

viewAsList : Response -> Int -> List (Html Msg)
viewAsList response idx =
    [ div [ class "historyPane" ]
        [ ol [ class "objectList" ] <| List.indexedMap (viewSummary idx) response.history
        , div [ class "detail", id "content" ]
            [ button [class "arrowButton", onClick NavigateDiffBack] [arrow "leftArrow"] ,
              div [class "diffPanel"] ( firstVersion response.stamp <| response.history !! idx ),
              button [class "arrowButton", onClick NavigateDiffForward] [arrow "rightArrow"]
            ]
        ]
    ]

viewSummary : Int -> Int -> History -> Html Msg
viewSummary sel idx h = li
        [ class (if sel == idx then "selected" else "" )
        , onClick (Select idx) ]
        [ span [ class "handle" ] [ text h.identifier.handle ] ]

firstVersion : Date -> Maybe History -> List (Html Msg)
firstVersion now mh = case mh of
    Nothing -> [ text "" ]
    Just h  -> [ {- viewTimeline now h, -} viewVersions (mkCtx now h) h.versions ]

viewVersions : Context -> List Version -> Html Msg
viewVersions ctx vs =
    let versions = List.reverse <| List.sortBy (\v -> Date.toTime v.from) vs
        paired   = List.map2 (,) (List.map Just (List.drop 1 versions) ++ [Nothing]) versions
    in div [ class "versions" ] (List.map (uncurry (viewVersion ctx)) paired)

viewVersion : Context -> Maybe Version -> Version -> Html Msg
viewVersion ctx was is =
    let rWas = Maybe.map (Rdap.render ctx.identifier << .object) was
        rIs  = Rdap.render ctx.identifier is.object
    in  div [ class "version" ]
            [ viewPeriod ctx.today is.from is.until
            , div [ class "rdap" ] [ Rdap.output <| Rdap.diff rWas rIs ] ]

friendlyDate : Date -> Date -> Html a
-- friendlyDate now dt = abbr [ title (toString dt) ] [ text <| relativeSpan now dt ]
friendlyDate now dt = abbr [ title (toString dt) ] [ text <| formatUtc config "%a %d %b %Y" dt ]

viewPeriod : Date -> Date -> Maybe Date -> Html a
viewPeriod now f mu = case mu of
    Nothing -> span [] [ text "From ", friendlyDate now f, text " to the present" ]
    Just u  -> span [] [ text "From ", friendlyDate now f, text " to ", friendlyDate now u ]

arrow : String -> Html a
arrow svgClass =
    svg [width "50", height "100", viewBox "0 0 50 100", Svg.Attributes.class svgClass]
        [path [strokeWidth "8", fill "transparent" , strokeLinecap "round", strokeLinejoin "round",
                   Svg.Attributes.d "M 10 10 L 40 50 L 10 90"] []]
