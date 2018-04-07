module Main exposing (..)

import Html exposing (Html, text, div, h1, img, a)
import Html.Attributes exposing (src, href, target)

import Server.Config as SC
import Server.RequestUtils as SR

import RemoteData exposing (WebData, RemoteData(..))


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
    {context : SC.Context
    ,remoteResponse : String
    }

initialModel : Model
initialModel =
    {context =
       {apiBaseUrl = "http://localhost:8080"}
    , remoteResponse = ""
    }

initialCmds : Cmd Msg
initialCmds =
  Cmd.batch
    [Cmd.map HandleResponse (SR.getRequestString initialModel.context "/" |> RemoteData.sendRequest)
    ]

init : ( Model, Cmd Msg )
init =
    ( initialModel
    , initialCmds
    )

---- UPDATE ----

type Msg
    = HandleResponse (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleResponse remoteResponse ->
            case remoteResponse of
                Success a ->
                    ( {model | remoteResponse = "SUCCESSFULLY RETRIEVED: " ++ a} , Cmd.none )
                Loading ->
                    ( {model | remoteResponse = "LOADING....."} , Cmd.none )
                Failure e ->
                    ( {model | remoteResponse = "Failed to load"} , Cmd.none )
                NotAsked ->
                    ( {model | remoteResponse = "Not Asked"} , Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/haskstarLogo.png" ] []
        , img [ src "/logo.svg" ] []
        , h1 [] [ text "Create Haskstar App!" ]
        , div [] [text <| "Server Response (localhost:8080/) " ++ model.remoteResponse]
        , a [href "localhost:8080/swagger-ui", target "_blank"] [text "Click here to see all API endpoints (localhost:8080/swagger-ui)"]
        ]




