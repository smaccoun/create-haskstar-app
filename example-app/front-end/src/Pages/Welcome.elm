module Pages.Welcome exposing (..)

import Bulma.Elements as Elements
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (Size(..))
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, style, target)
import Link
import RemoteData exposing (RemoteData(..), WebData)
import Server.Config as SC
import Server.RequestUtils exposing (getRequestString)


type alias Model =
    WebData String


init : SC.Context -> ( Model, Cmd Msg )
init context =
    ( NotAsked
    , Cmd.map ReceiveResponse
        (getRequestString context "health"
            |> RemoteData.sendRequest
        )
    )


type Msg
    = ReceiveResponse (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg response =
    case msg of
        ReceiveResponse response ->
            response ! []


viewWelcomeScreen : Model -> Html msg
viewWelcomeScreen response =
    div []
        [ hero { heroModifiers | size = Small, color = Bulma.Modifiers.Light }
            []
            [ heroBody []
                [ fluidContainer [ style [ ( "display", "flex" ), ( "justify-content", "center" ) ] ]
                    [ Elements.easyImage Elements.Natural [ style [ ( "width", "300px" ) ] ] "/haskstarLogo.png"
                    ]
                ]
            ]
        , section NotSpaced
            []
            [ Elements.title Elements.H2 [] [ text "Server Connection" ]
            , div []
                [ case response of
                    Success r ->
                        text <| "Server Response (localhost:8080/) " ++ r

                    NotAsked ->
                        div [] [ text "Have not yet contacted server" ]

                    Loading ->
                        div [] [ text "Loading..." ]

                    Failure e ->
                        div [] [ text <| "Error loading from server" ++ toString e ]
                ]
            , a [ href "http://localhost:8080/swagger-ui", target "_blank" ] [ text "Click here to see all API endpoints (localhost:8080/swagger-ui)" ]
            ]
        ]
