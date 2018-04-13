module Types.Login exposing (..)

import Http
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)
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


type alias LoginResponse =
    { jwtToken : String
    , userId : String
    }


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    decode LoginResponse
        |> required "jwtToken" string
        |> required "userId" string
