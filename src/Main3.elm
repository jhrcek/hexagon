module Main3 exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as Attr


main : Program () () ()
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view model =
    -- We'll put the math content inside a container div with an id.
    -- KaTeX will later render this LaTeX string into a proper formula.
    div []
        [ div [ Attr.id "math-container" ] [ text "\\binom{15}{12}" ] ]
