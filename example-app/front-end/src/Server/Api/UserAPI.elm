module Server.Api.UserAPI exposing (..)

import RemoteData exposing (WebData)
import Server.Config exposing (apiUrl)
import Server.ResourceAPI exposing (..)
import Types.User exposing (User, userDecoder)


userEndpoint : Endpoint
userEndpoint =
    Endpoint "user"


getUsers : Server.Config.Context -> Cmd (WebData User)
getUsers context =
    getContainer context userEndpoint userDecoder
