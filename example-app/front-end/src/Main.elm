module Main exposing (..)

import Bulma.CDN exposing (..)
import Bulma.Components exposing (navbar, navbarEnd, navbarItemLink, navbarLink, navbarMenu, navbarModifiers)
import Components.Navbar exposing (viewNavbar)
import Html exposing (Html, a, div, h1, img, main_, text)
import Link
import Navigation
import Pages.Admin.Index as AdminIndex
import Pages.Index exposing (AppPage(..), AppPageMsg(..), locationToPage)
import Pages.LoginPage as LoginPage
import Pages.Welcome exposing (viewWelcomeScreen)
import Ports exposing (receiveToken, saveToken)
import RemoteData exposing (RemoteData(..), WebData)
import Server.Config as SC
import Server.RequestUtils as SR
import Task


---- PROGRAM ----


type alias Flags =
    { environment : String
    , apiBaseUrl : String
    , jwtToken : Maybe String
    }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { context : SC.Context
    , remoteResponse : String
    , currentPage : AppPage
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        { apiBaseUrl, jwtToken } =
            flags

        initialContext =
            { apiBaseUrl = apiBaseUrl, jwtToken = jwtToken }

        ( initPage, initPageCmd ) =
            locationToPage initialContext location

        model =
            { context = initialContext
            , remoteResponse = ""
            , currentPage = initPage
            }

        initialCmds =
            Cmd.batch
                [ Cmd.map HandleResponse (SR.getRequestString model.context "" |> RemoteData.sendRequest)
                , Cmd.map PageMsgW initPageCmd
                ]
    in
    ( model
    , initialCmds
    )



---- UPDATE ----


type Msg
    = UrlChange Navigation.Location
    | NewUrl String
    | HandleResponse (WebData String)
    | PageMsgW AppPageMsg
    | ReceiveToken String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                ( newPage, newPageCmd ) =
                    locationToPage model.context location
            in
            ( { model | currentPage = newPage }, Cmd.map PageMsgW newPageCmd )

        NewUrl destination ->
            let
                ( newUrlModel, cMsg ) =
                    Link.navigate model destination
            in
            ( newUrlModel, cMsg )

        HandleResponse remoteResponse ->
            case remoteResponse of
                Success a ->
                    ( { model | remoteResponse = "SUCCESSFULLY RETRIEVED: " ++ a }, Cmd.none )

                Loading ->
                    ( { model | remoteResponse = "LOADING....." }, Cmd.none )

                Failure e ->
                    ( { model | remoteResponse = "Failed to load" }, Cmd.none )

                NotAsked ->
                    ( { model | remoteResponse = "Not Asked" }, Cmd.none )

        ReceiveToken jwtToken ->
            let
                curContext =
                    model.context

                updatedContext =
                    { curContext | jwtToken = Just jwtToken }
            in
            { model | context = updatedContext }
                ! [ Task.perform (always (NewUrl "/admin/home")) (Task.succeed ()) ]

        PageMsgW pageMsg ->
            let
                ( page, pageCmd ) =
                    Pages.Index.update pageMsg model.currentPage
            in
            { model | currentPage = page }
                ! [ Cmd.map PageMsgW pageCmd ]



---- VIEW ----


view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , viewNavbar True True model.currentPage NewUrl
        , Html.map PageMsgW <| Pages.Index.view model.currentPage
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveToken ReceiveToken ]
