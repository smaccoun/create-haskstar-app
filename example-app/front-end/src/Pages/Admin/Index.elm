module Pages.Admin.Index exposing (..)

import Html exposing (Html)
import Pages.Admin.Home as Home
import Server.Config
import UrlParser as Url exposing ((</>), (<?>), s, top)


type AdminPage
    = AdminHome Home.Model


type AdminRoute
    = AdminHomeRoute


type AdminPageMsg
    = HomeMsg Home.Msg


initializePageFromRoute : Server.Config.Context -> AdminRoute -> ( AdminPage, Cmd AdminPageMsg )
initializePageFromRoute serverContext route =
    case route of
        AdminHomeRoute ->
            let
                ( initialPage, initialCmd ) =
                    Home.init serverContext
            in
            ( AdminHome initialPage, Cmd.map HomeMsg initialCmd )


update : AdminPage -> AdminPageMsg -> ( AdminPage, Cmd AdminPageMsg )
update adminPage msg =
    case msg of
        HomeMsg homeMsg ->
            case adminPage of
                AdminHome homeModel ->
                    let
                        ( updatedPage, pageCmd ) =
                            Home.update homeModel homeMsg
                    in
                    ( AdminHome updatedPage, Cmd.map HomeMsg pageCmd )


viewAdminPage : AdminPage -> Html AdminPageMsg
viewAdminPage adminPage =
    case adminPage of
        AdminHome homeModel ->
            Html.map HomeMsg <| Home.view homeModel


routes : List (Url.Parser (AdminRoute -> a) a)
routes =
    [ Url.map AdminHomeRoute (s "admin" </> s "home")
    ]
