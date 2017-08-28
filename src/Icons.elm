module Icons exposing (..)

import Html exposing (div, Html, ol, li, span, text, button, select, option)
import Svg exposing (svg, path)
import Svg.Attributes as SvgA
import Svg.Events as SvgE

import Model exposing (..)

arrow : String -> Html a
arrow svgClass =
    svg [SvgA.viewBox "0 0 50 100", SvgA.class svgClass]
        [path [SvgA.strokeWidth "8", SvgA.fill "transparent" , SvgA.strokeLinecap "round", SvgA.strokeLinejoin "round",
                   SvgA.d "M 10 10 L 40 50 L 10 90"] []]

moreIcon : String -> Html a
moreIcon svgClass =
    svg [SvgA.viewBox "0 0 100 100", SvgA.class svgClass] [
        Svg.defs [] [Svg.mask [SvgA.id svgClass] [
                Svg.rect [SvgA.x "0", SvgA.y "0", SvgA.width "100",
                              SvgA.height "100", SvgA.fill "white"] [],
                Svg.circle [SvgA.cx "50", SvgA.cy "50", SvgA.r "13.33",
                                SvgA.fill "black"] [],
                Svg.circle [SvgA.cx "18.33", SvgA.cy "50", SvgA.r "13.33",
                                SvgA.fill "black"] [],
                Svg.circle [SvgA.cx "81.66", SvgA.cy "50", SvgA.r "13.33",
                                SvgA.fill "black"] []
                ]
          ],
        Svg.circle [SvgA.cx "50", SvgA.cy "50", SvgA.r "50",
                        SvgA.mask <| "url(#" ++ svgClass ++ ")"] []
    ]

expandIcon : String -> Html a
expandIcon svgClass =
    svg [SvgA.viewBox "0 0 100 100", SvgA.class svgClass] [
        Svg.defs [] [Svg.mask [SvgA.id svgClass] [
                Svg.rect [SvgA.x "0", SvgA.y "0", SvgA.width "100",
                              SvgA.height "100", SvgA.fill "white"] [],
                path [SvgA.strokeWidth "17", SvgA.fill "transparent" , SvgA.strokeLinecap "round",
                          SvgA.strokeLinejoin "round", SvgA.d "M 20 35 L 50 65 L 80 35", SvgA.stroke "black"] []
              ]
          ],
        Svg.circle [SvgA.cx "50", SvgA.cy "50", SvgA.r "50",
                        SvgA.mask <| "url(#" ++ svgClass ++ ")"] []
    ]

-- Note: We need to change the mask name otherwise Chrome won't update the icon. Only updating the mask's path
--       is not enough.
lockerIcon : LockerState -> String -> String -> Html a
lockerIcon state svgClass id =
    let maskPathDraw = case state of
                           Locked   -> "M 35 50 v -20 c 0 -20 30 -20 30 0 v 20"
                           Unlocked -> "M 35 50 v -20 c 0 -20 30 -20 30 0"
        maskName = toString state ++ id
        iconTitle = if state == Locked then "Unlock version" else "Lock version"
    in svg [SvgA.viewBox "0 0 100 100", SvgA.class svgClass] [
           Svg.defs [] [Svg.mask [SvgA.id maskName] [
                   Svg.rect [SvgA.x "0", SvgA.y "0", SvgA.width "100",
                                 SvgA.height "100", SvgA.fill "white"] [],
                   Svg.rect [SvgA.x "25", SvgA.y "45", SvgA.width "50",
                                 SvgA.height "30", SvgA.rx "10", SvgA.ry "10", SvgA.fill "black"] [],
                   Svg.path [SvgA.d maskPathDraw, SvgA.strokeWidth "6", SvgA.strokeLinecap "round",
                                 SvgA.strokeLinejoin "round", SvgA.fill "transparent", SvgA.stroke "black"] []
                   ]
             ],
            Svg.circle [SvgA.cx "50", SvgA.cy "50", SvgA.r "50",
                           SvgA.mask <| "url(#" ++ maskName ++ ")"] [Svg.title [] [text iconTitle]]
       ]

zoomIcon : String -> Html a
zoomIcon svgClass =
    Svg.svg [SvgA.viewBox "0 0 24 24", SvgA.class svgClass]
        [Svg.path [SvgA.strokeLinecap "round", SvgA.strokeLinejoin "round",
                   SvgA.d "M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"] []]

zoomOutIcon : Html a
zoomOutIcon =
    Svg.svg [SvgA.viewBox "0 0 24 24", SvgA.class "zoomOutIcon"]
        [Svg.path [SvgA.strokeLinecap "round", SvgA.strokeLinejoin "round",
                   SvgA.d "M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z M 3 4 h 10"] [Svg.title [] [text "Zoom out"]],
            Svg.rect [SvgA.x "6", SvgA.y "8.5", SvgA.width "7", SvgA.height "2"][Svg.title [] [text "Zoom out"]]]

zoomOutCircleIcon : Html a
zoomOutCircleIcon =
    Svg.svg [SvgA.viewBox "0 0 24 24", SvgA.class "zoomOutIcon"]
        [
           Svg.defs [] [Svg.mask [SvgA.id "zoomOutMask"] [
                   Svg.rect [SvgA.x "0", SvgA.y "0", SvgA.width "24",
                                 SvgA.height "24", SvgA.fill "white"] [],
                    Svg.path [SvgA.strokeLinecap "round", SvgA.strokeLinejoin "round", SvgA.fill "black",
                            SvgA.d "M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z M 3 4 h 10"] [],
                    Svg.rect [SvgA.x "6", SvgA.y "8.5", SvgA.width "7", SvgA.height "2", SvgA.fill "black"][]]],

           Svg.circle [SvgA.cx "12", SvgA.cy "12", SvgA.r "12",
                           SvgA.mask <| "url(#zoomOutMask)"] [Svg.title [] [text "Zoom out"]]]

zoomBackIcon : Html a
zoomBackIcon =
    let svgClass = "zoomBackIcon"
    in Svg.svg [SvgA.viewBox "0 0 1400 1400", SvgA.class svgClass] [
        Svg.defs [] [Svg.mask [SvgA.id svgClass] [
                Svg.rect [SvgA.x "0", SvgA.y "0", SvgA.width "1400", SvgA.height "1400", SvgA.fill "white"] [],
                path [SvgA.strokeWidth "17", SvgA.fill "black" , SvgA.strokeLinecap "round", SvgA.strokeLinejoin "round",
                          SvgA.d "M 500,304.6 V 147.8 c 0,-15.7 -6,-31.4 -17.9,-43.4 -11.9,-11.9 -27.6,-17.9 -43.3,-17.9 -15.7,0 -31.4,5.9 -43.3,17.9 L 10,484.7 395.4,864.9 c 11.9,11.9 27.6,17.9 43.3,17.9 15.7,0 31.4,-6 43.3,-17.9 11.9,-11.9 17.9,-27.7 17.9,-43.3 V 669.1 c 168.4,4.2 352.5,34.7 490,244.3 V 852.1 C 990,568.4 775.6,335.1 500,304.6 Z",
                          SvgA.stroke "black", SvgA.transform "translate(200, 150)"] []
              ]
          ],
        Svg.circle [SvgA.cx "700", SvgA.cy "700", SvgA.r "700", SvgA.mask <| "url(#" ++ svgClass ++ ")"] [Svg.title [] [Svg.text "Zoom back"]]
    ]

markerButton : Bool -> Maybe Msg -> String -> String -> Html Msg
markerButton selected mMsg style tooltip =
    let svgClass = if selected == True then " selected" else ""
        action = case mMsg of
                     Nothing -> []
                     Just msg -> [SvgE.onClick msg]
    in svg [SvgA.viewBox "0 0 20 50", SvgA.class ("timelineMarker " ++ svgClass), SvgA.style style]
           [
              Svg.line ([SvgA.x1 "10", SvgA.y1 "20", SvgA.x2 "10", SvgA.y2 "50",
                             SvgA.class "timelineMarkerItem"] ++ action) [Svg.title [] [Svg.text tooltip]],
              Svg.circle ([SvgA.cx "10", SvgA.cy "10", SvgA.r "7", SvgA.style "stroke-width: 6",
                             SvgA.class "timelineMarkerItem"] ++ action) [Svg.title [] [Svg.text tooltip]]
           ]
