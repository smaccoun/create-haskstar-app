module Server.ResourceAPI exposing (..)

import Http exposing (jsonBody)
import Json.Decode exposing (list)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import Server.Config exposing (Endpoint(..))
import Server.RequestUtils exposing (BaseRequestParams(..), getRequest, postRequest)


type alias RemoteCmd a =
    Cmd (WebData a)


getContainer : BaseRequestParams a -> RemoteCmd (List a)
getContainer (BaseRequestParams context endpoint decoder) =
    getRequest context
        endpoint
        (list decoder)
        |> RemoteData.sendRequest


getItem : BaseRequestParams a -> String -> RemoteCmd a
getItem (BaseRequestParams context (Endpoint endpoint) decoder) uuid =
    getRequest context
        (Endpoint <| endpoint ++ "/" ++ uuid)
        decoder
        |> RemoteData.sendRequest


createItem : BaseRequestParams a -> Value -> RemoteCmd a
createItem (BaseRequestParams context endpoint decoder) encodedValue =
    postRequest context
        endpoint
        (jsonBody encodedValue)
        decoder
        |> RemoteData.sendRequest


updateItem : BaseRequestParams a -> Value -> String -> RemoteCmd a
updateItem (BaseRequestParams context (Endpoint endpoint) decoder) encodedValue uuid =
    postRequest context
        (Endpoint <| endpoint ++ "/update/" ++ uuid)
        (jsonBody encodedValue)
        decoder
        |> RemoteData.sendRequest
