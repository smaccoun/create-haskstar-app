module Main exposing (..)

import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Elements as Elements
import Bulma.Layout exposing (..)
import Form exposing (Form)
import Html exposing (Html, a, div, h1, img, main_, text)
import Html.Attributes exposing (href, src, style, target)
import Pages.Index exposing (AppPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Server.Api.AuthAPI exposing (performLogin)
import Server.Config as SC
import Server.RequestUtils as SR
import Views.LoginPanel as LoginPanel


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    { context : SC.Context
    , remoteResponse : String
    , currentPage : AppPage
    }


initialModel : Model
initialModel =
    { context =
        { apiBaseUrl = "http://localhost:8080", jwtToken = Nothing }
    , remoteResponse = ""
    , currentPage = LoginPage (Form.initial [] LoginPanel.validation)
    }


initialCmds : Cmd Msg
initialCmds =
    Cmd.batch
        [ Cmd.map HandleResponse (SR.getRequestString initialModel.context "/" |> RemoteData.sendRequest)
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , initialCmds
    )



---- UPDATE ----


type Msg
    = HandleResponse (WebData String)
    | PageMsgW PageMsg
    | ReceiveLogin (WebData String)


type PageMsg
    = LoginPageMsg Form.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "MSG: " msg of
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

        ReceiveLogin loginResponse ->
            case loginResponse of
                Success jwtToken ->
                    let
                        curContext =
                            model.context

                        newContext =
                            { curContext | jwtToken = Just jwtToken }
                    in
                    ( { model | context = newContext }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PageMsgW pageMsg ->
            case pageMsg of
                LoginPageMsg loginPageMsg ->
                    case model.currentPage of
                        LoginPage loginPageModel ->
                            let
                                ( newModel, cmd ) =
                                    updateLoginPage loginPageMsg loginPageModel model

                                _ =
                                    Debug.log "CMD: " cmd
                            in
                            ( newModel, cmd )


updateLoginPage : Form.Msg -> Form () LoginPanel.LoginForm -> Model -> ( Model, Cmd Msg )
updateLoginPage formMsg formModel model =
    case Debug.log "FORM MSG: " formMsg of
        Form.Submit ->
            let
                o =
                    Debug.log "FM: " <| formModel

                submitCmd =
                    case Form.getOutput formModel of
                        Just fModel ->
                            Cmd.map ReceiveLogin <| performLogin fModel model.context

                        Nothing ->
                            Cmd.none

                c =
                    Debug.log "SM: " submitCmd
            in
            ( model, submitCmd )

        _ ->
            let
                newLoginPageModel =
                    Form.update LoginPanel.validation formMsg formModel

                z =
                    Debug.log "NEW LOGIN: " newLoginPageModel
            in
            ( { model | currentPage = LoginPage newLoginPageModel }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , fluidContainer [ style [ ( "width", "300px" ) ] ] [ Elements.easyImage Elements.Natural [] "/haskstarLogo.png" ]
        , h1 [] [ text "Create Haskstar App!" ]
        , div [] [ text <| "Server Response (localhost:8080/) " ++ model.remoteResponse ]
        , a [ href "http://localhost:8080/swagger-ui", target "_blank" ] [ text "Click here to see all API endpoints (localhost:8080/swagger-ui)" ]
        , case model.currentPage of
            LoginPage loginPageModel ->
                Html.map (\m -> PageMsgW (LoginPageMsg m)) <| LoginPanel.view loginPageModel
        ]
