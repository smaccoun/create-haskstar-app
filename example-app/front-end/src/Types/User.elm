module Types.User exposing (..)

import Json.Decode exposing (string)


type alias User =
    { userId : String
    , email : String
    }


userDecoder : Json.Decoder User
userDecoder =
    decode User
        |> required "userId" string
        |> required "email" string
