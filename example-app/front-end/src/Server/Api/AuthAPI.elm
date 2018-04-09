module Server.Api.AuthAPI exposing (..)

import Json.Decode as D exposing (Decoder, string)
import RemoteData exposing (WebData)
import Server.Config exposing (apiUrl)
import Server.RequestUtils exposing (postRequest)
import Types.Login exposing (LoginForm, loginFormEncoder)


loginEndpoint : Server.Config.Endpoint
loginEndpoint =
    "login"


performLogin : LoginForm -> Server.Config.Context -> Cmd (WebData String)
performLogin loginForm context =
    postRequest context
        loginEndpoint
        (loginFormEncoder loginForm)
        string
        |> RemoteData.sendRequest
