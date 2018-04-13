module Pages.LoginPage exposing (..)

import Components.LoginPanel as LoginPanel
import Bulma.Layout exposing (section, SectionSpacing(..))
import Html exposing (Html, div, text)

view : (LoginPanel.Msg -> msg) -> LoginPanel.Model -> Html msg
view msgMap loginPageModel =
    section NotSpaced
        []
        [ div [] [ text "You can login to an admin account by using username 'admin@haskstar.com' and password 'haskman'" ]
        , Html.map msgMap <| LoginPanel.view loginPageModel
        ]
