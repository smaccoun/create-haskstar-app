module Components.Navbar exposing (..)

import Bulma.Components exposing (..)
import Html exposing (Html, text)
import Link
import Pages.Index exposing (AppPage(..))


viewNavbar : Bool -> Bool -> AppPage -> (String -> msg) -> Html msg
viewNavbar isMenuOpen isMenuDropdownOpen curPage newUrlMsg =
    let
        matchesPage linkPage =
            case curPage of
                LoginPage _ ->
                    LoginPage == linkPage

                _ ->
                    False
    in
    navbar navbarModifiers
        []
        [ navbarMenu isMenuOpen
            []
            [ navbarEnd []
                [ navbarItemLink (matchesPage LoginPage) [ Link.link (newUrlMsg "/login") ] [ text "Login" ]
                ]
            ]
        ]
