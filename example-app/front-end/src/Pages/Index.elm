module Pages.Index exposing (..)

import Components.LoginPanel as LoginPanel
import Navigation exposing (Location)
import Pages.Admin.Index as Admin exposing (AdminRoute(..))
import Server.Config
import UrlParser as Url exposing ((</>), (<?>), s, top)


type AppPage
    = Error404
    | WelcomeScreen
    | LoginPage LoginPanel.Model
    | AdminPageW Admin.AdminPage


type AppPageMsg
    = LoginPageMsg LoginPanel.Msg
    | AdminPageMsg Admin.AdminPageMsg


type Route
    = Welcome
    | Login
    | AdminRouteW AdminRoute


initializePageFromRoute : Server.Config.Context -> Route -> ( AppPage, Cmd AppPageMsg )
initializePageFromRoute serverContext route =
    case route of
        Welcome ->
            ( WelcomeScreen, Cmd.none )

        Login ->
            ( LoginPage (LoginPanel.init serverContext), Cmd.none )

        AdminRouteW adminRoute ->
            let
                ( adminPage, adminPageCmd ) =
                    Admin.initializePageFromRoute serverContext adminRoute
            in
            ( AdminPageW adminPage, Cmd.map AdminPageMsg adminPageCmd )


locationToPage : Server.Config.Context -> Location -> ( AppPage, Cmd AppPageMsg )
locationToPage serverContext location =
    Url.parsePath routes location
        |> Maybe.map (initializePageFromRoute serverContext)
        |> Maybe.withDefault ( Error404, Cmd.none )


routes : Url.Parser (Route -> a) a
routes =
    let
        unprotected =
            [ Url.map Welcome top
            , Url.map Login (s "login")
            , Url.map (AdminRouteW Admin.AdminHomeRoute) (s "admin" </> s "home")
            ]

        adminRoutes =
            List.map (Url.map AdminRouteW) Admin.routes
    in
    Url.oneOf <| List.concatMap identity [ unprotected, adminRoutes ]


update : AppPageMsg -> AppPage -> ( AppPage, Cmd AppPageMsg )
update pageMsg currentPage =
    case pageMsg of
        LoginPageMsg loginPageMsg ->
            case currentPage of
                LoginPage loginPageModel ->
                    let
                        ( uLoginModel, cmd ) =
                            LoginPanel.update loginPageMsg loginPageModel
                    in
                    ( LoginPage uLoginModel, Cmd.map LoginPageMsg cmd )

                _ ->
                    ( currentPage, Cmd.none )

        AdminPageMsg adminPageMsg ->
            case currentPage of
                AdminPageW adminPage ->
                    let
                        ( updatedAdminPage, adminPageCmd ) =
                            Admin.update adminPage adminPageMsg
                    in
                    ( AdminPageW updatedAdminPage
                    , Cmd.map AdminPageMsg adminPageCmd
                    )

                _ ->
                    ( currentPage, Cmd.none )
