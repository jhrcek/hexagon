module Main exposing (main)

import Browser
import Html exposing (Html)
import Svg exposing (Svg, svg, polygon)
import Svg.Attributes exposing (width, height, viewBox, points, fill, stroke)
import String


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view = view
        }


hexagon : Float -> Float -> Float -> Svg msg
hexagon cx cy r =
    let
        sqrt3 = 1.7320508075688772
        x1 = cx
        y1 = cy - r

        x2 = cx + (sqrt3 / 2) * r
        y2 = cy - r / 2

        x3 = cx + (sqrt3 / 2) * r
        y3 = cy + r / 2

        x4 = cx
        y4 = cy + r

        x5 = cx - (sqrt3 / 2) * r
        y5 = cy + r / 2

        x6 = cx - (sqrt3 / 2) * r
        y6 = cy - r / 2

        pts =
            String.join " "
                [ String.fromFloat x1 ++ "," ++ String.fromFloat y1
                , String.fromFloat x2 ++ "," ++ String.fromFloat y2
                , String.fromFloat x3 ++ "," ++ String.fromFloat y3
                , String.fromFloat x4 ++ "," ++ String.fromFloat y4
                , String.fromFloat x5 ++ "," ++ String.fromFloat y5
                , String.fromFloat x6 ++ "," ++ String.fromFloat y6
                ]
    in
    polygon [ points pts, fill "none", stroke "black" ] []


view : () -> Html ()
view _ =
    let
        r = 50
        sqrt3 = 1.7320508075688772
        cx1 = 150
        cy1 = 150
        cx2 = cx1 + (sqrt3 * r)
        cy2 = 150
        cx3 = (cx1 + cx2) / 2
        cy3 = cy1 - (1.5 * r)
    in
    svg [ width "300", height "300", viewBox "0 0 300 300" ]
        [ hexagon cx1 cy1 r
        , hexagon cx2 cy2 r
        , hexagon cx3 cy3 r
        ]
