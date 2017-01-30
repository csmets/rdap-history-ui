module Render exposing (viewAsList)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onWithOptions, onInput, onClick)
-- import Html.Lazy exposing (lazy)

import Model exposing (..)
import Rdap
import Relativity exposing (..)

-- TODO figure out if needed, and move to common module if so
type alias Response =
    { stamp : Date.Date
    , history : List History
    }

viewAsList : Response -> Int -> List (Html Msg)
viewAsList response idx =
    [ div [ class "historyPane" ]
        [ ol [ class "objectList" ] <| List.indexedMap (viewSummary idx) response.history
        , div [class "detail", id "content" ]
            ( firstVersion response.stamp <| List.head (List.drop idx response.history) )
        ]
    ]

firstVersion : Date.Date -> Maybe History -> List (Html Msg)
firstVersion now mh = case mh of
    Nothing -> [ text "" ]
    Just h  -> [ {- viewTimeline now h, -} viewVersions now h ]

viewSummary : Int -> Int -> History -> Html Msg
viewSummary sel idx h = li
        [ class (if sel == idx then "selected" else "" )
        , onClick (Select idx) ]
        [ span [ class "handle" ] [ text h.handle ] ]

viewPeriod : Date -> Date -> Maybe Date -> Html a
viewPeriod now f mu = case mu of
    Nothing -> text ("From " ++ relativeSpan now f ++ " to the present")
    Just u  -> text ("From " ++ relativeSpan now f ++ " to " ++ relativeSpan now u)

viewVersions : Date -> History -> Html a
viewVersions now h = let sv = List.reverse <| List.sortBy (\v -> Date.toTime v.from) h.versions
                  in div [ class "versions" ] (List.map (viewVersion now Nothing) sv)

viewVersion : Date -> Maybe Version -> Version -> Html a
viewVersion now was is = div [ class "version" ]
                    [ viewPeriod now is.from is.until
                    , div [ class "rdap" ] (Rdap.render is) ]
