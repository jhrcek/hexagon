module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, polygon, text_)
import Svg.Attributes exposing (viewBox, points, fill, stroke, x, y, textAnchor, dominantBaseline)
import String


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { w : Float
    , h : Float
    , r : Float
    }


initialModel : Model
initialModel =
    { w = 800
    , h = 600
    , r = 50
    }


type Msg
    = Resize Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize Resize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newW newH ->
            ( { model | w = toFloat newW, h = toFloat newH }, Cmd.none )


hexagonWithText : Float -> Float -> Float -> String -> Svg msg
hexagonWithText cx cy r txt =
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
    Svg.g []
        [ polygon [ points pts, fill "none", stroke "black" ] []
        , text_
            [ x (String.fromFloat cx)
            , y (String.fromFloat (cy + 5)) -- +5 to center text vertically better
            , textAnchor "middle"
            , dominantBaseline "middle"
            ]
            [ Svg.text txt ]
        ]


pascalNumber : Int -> Int -> Int
pascalNumber row col =
    if col == 0 || col == row then
        1
    else
        pascalNumber (row - 1) (col - 1) + pascalNumber (row - 1) col


pyramid : Float -> Float -> Float -> List (Svg msg)
pyramid w h r =
    let
        sqrt3 = 1.7320508075688772
        rowHeight = 1.5 * r
        nRows = floor (h / rowHeight)

        rowSvg i =
            let
                rowCount = i + 1
                -- Position from the top; top row i=0 at y=r
                -- Invert so that top row is at top of screen:
                -- Actually we want a pyramid with base at bottom?
                -- Let's keep top at top:
                y = r + toFloat i * rowHeight
                firstX = (w / 2) - ((toFloat (rowCount - 1)) * sqrt3 * r / 2)
            in
            List.map (\col ->
                let
                    x = firstX + toFloat col * sqrt3 * r
                    number = pascalNumber i col
                in
                hexagonWithText x y r (String.fromInt number)
            ) (List.range 0 (rowCount - 1))
    in
    -- Just draw as many rows as fit
    List.concatMap rowSvg (List.range 0 (nRows - 1))


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0"
        , style "padding" "0"
        , style "overflow" "hidden"
        ]
        [ svg
            [ style "display" "block"
            , style "width" "100vw"
            , style "height" "100vh"
            , viewBox ("0 0 " ++ String.fromFloat model.w ++ " " ++ String.fromFloat model.h)
            ]
            (pyramid model.w model.h model.r)
        ]
