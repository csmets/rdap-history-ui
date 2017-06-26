module Render exposing (viewAsList)

import Date exposing (Date)
import Date.Extra.Compare exposing (Compare2(..), is)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format exposing (formatUtc, isoDateFormat)
import Html exposing (..)
import Html.Attributes exposing (class, value, id, title, disabled)
import Html.Events exposing (onWithOptions, onInput, onClick)
import List exposing (length)
import List.Extra exposing ((!!), dropWhile, last)
import Maybe.Extra exposing (isJust, (?))
import Svg exposing (svg, path)
import Svg.Attributes exposing (width, height, viewBox, strokeLinecap, strokeLinejoin, strokeWidth, fill)

import Model exposing (..)
import Rdap

-- Rendering happens inside a context
type alias Context =
    { today : Date -- TODO: this can probably be removed
    , identifier : Identifier
    , modificationDate : Date
    }

mkCtx : Date -> History -> Date -> Context
mkCtx today history modDate = Context today history.identifier modDate

viewAsList : Response -> Int -> Date -> List (Html Msg)
viewAsList response idx modDate =
    [ div [ class "historyPane" ] ((objectListPanel response.history idx) ++ (detailPanel response idx modDate))]

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

detailPanel : Response -> Int -> Date -> List (Html Msg)
detailPanel response idx modDate =
    let history = response.history !! idx
    in [ div [ class "detail", id "content" ]
          [ button ([class "arrowButton", onClick NavigateDiffBack] ++ checkNavBack history modDate) [arrow "leftArrow"] ,
            div [class "diffPanel"] ( diffPanel response.stamp history modDate ),
            button ([class "arrowButton", onClick NavigateDiffForward] ++ checkNavFwd history modDate) [arrow "rightArrow"]
          ]
       ]

diffPanel : Date -> Maybe History -> Date -> List (Html Msg)
diffPanel now mh modDate = case mh of
    Nothing -> [ text "" ]
    Just h  -> [ {- viewTimeline now h, -} viewModification (mkCtx now h modDate) h.versions ]

viewModification : Context -> List Version -> Html Msg
viewModification ctx vs =
    let splited = dropWhile (\v -> is After v.from ctx.modificationDate) vs
        isVersion = splited !! 0
        wasVersion = splited !! 1
        versions = Maybe.Extra.values [wasVersion, isVersion]
        paired   = List.map2 (,) (Nothing :: List.map Just versions) versions
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

checkNavBack : Maybe History -> Date -> List (Html.Attribute Msg)
checkNavBack history modDate = checkNav history modDate (\h -> h !! ((length h) - 2))

checkNavFwd : Maybe History -> Date -> List (Html.Attribute Msg)
checkNavFwd history modDate = checkNav history modDate List.head

checkNav : Maybe History -> Date -> (List Version -> Maybe Version) -> List (Html.Attribute Msg)
checkNav history modDate f =
    if Maybe.withDefault False <| Maybe.map ((/=) (Date.toTime modDate)) <| Maybe.map (Date.toTime << .from)
           <| Maybe.Extra.join <| Maybe.map f <| Maybe.map .versions history
    then []
    else [disabled True]
