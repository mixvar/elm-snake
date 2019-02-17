module Main exposing (main)

import Browser
import Html


main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \_ -> Html.text "Hello World"
        }
