module Server.Api.Index exposing (..)

import Server.Config exposing (Context, Endpoint(..))
import Server.RequestUtils exposing (BaseRequestParams(..))
import Types.User exposing (User, userDecoder)


userEndpoint : Endpoint
userEndpoint =
    Endpoint "users"


userApiResourceParams : Context -> BaseRequestParams User
userApiResourceParams context =
    BaseRequestParams context userEndpoint userDecoder


loginEndpoint : Endpoint
loginEndpoint =
    Endpoint "login"
