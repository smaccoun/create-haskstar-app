module Pages.Welcome exposing (..)

import Bulma.Elements as Elements
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (Size(..))
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, style, target)
import Link


viewWelcomeScreen : String -> (String -> msg) -> Html msg
viewWelcomeScreen remoteResponse newUrlMsg =
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
            , div [] [ text <| "Server Response (localhost:8080/) " ++ remoteResponse ]
            , a [ href "http://localhost:8080/swagger-ui", target "_blank" ] [ text "Click here to see all API endpoints (localhost:8080/swagger-ui)" ]
            ]
        , a [ Link.link (newUrlMsg "login") ] [ text "Go to login page" ]
        ]
