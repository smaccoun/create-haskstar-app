module Server.Api.UserAPI exposing (..)

import Json.Decode as D exposing (Decoder, string)
import RemoteData exposing (WebData)
import Server.Config exposing (apiUrl)
import Server.RequestUtils exposing (postRequest)
import Types.User exposing (userDecoder)


userEndpoint : Server.Config.Endpoint
userEndpoint =
    "user"


getUsers : userForm -> Server.Config.Context -> Cmd (WebData String)
getUsers userForm context =
    getRequest context
        userEndpoint
        userDecoder
        |> RemoteData.sendRequest
