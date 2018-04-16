module Pages.LoginPage exposing (..)

import Bulma.Layout exposing (SectionSpacing(..), section)
import Components.LoginPanel as LoginPanel
import Html exposing (Html, div, text)


view : LoginPanel.Model -> Html LoginPanel.Msg
view loginPageModel =
    section NotSpaced
        []
        [ div [] [ text "You can login to an admin account by using username 'admin@haskstar.com' and password 'haskman'" ]
        , LoginPanel.view loginPageModel
        ]
