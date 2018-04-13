module Server.Api.AuthAPI exposing (..)

import Json.Decode as D exposing (Decoder, string)
import RemoteData exposing (WebData)
import Server.Api.Index exposing (loginEndpoint)
import Server.Config exposing (apiUrl)
import Server.RequestUtils exposing (postRequest)
import Types.Login exposing (LoginForm, LoginResponse, loginFormEncoder, loginResponseDecoder)


performLogin : LoginForm -> Server.Config.Context -> Cmd (WebData LoginResponse)
performLogin loginForm context =
    postRequest context
        loginEndpoint
        (loginFormEncoder loginForm)
        loginResponseDecoder
        |> RemoteData.sendRequest
