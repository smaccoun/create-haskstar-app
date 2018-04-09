module Types.Login exposing (..)

import Http
import Json.Encode


type alias LoginForm =
    { email : String
    , password : String
    }


loginFormEncoder : LoginForm -> Http.Body
loginFormEncoder { email, password } =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "email", Json.Encode.string email )
            , ( "password", Json.Encode.string password )
            ]
