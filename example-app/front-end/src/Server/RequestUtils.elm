module Server.RequestUtils exposing (..)

import Http exposing (get)
import Json.Decode as Json
import Server.Config as S exposing (Endpoint(..))


type BaseRequestParams a
    = BaseRequestParams S.Context S.Endpoint (Json.Decoder a)


request : S.Context -> String -> String -> Http.Body -> Http.Expect a -> Http.Request a
request context method url body expect =
    Http.request
        { method = method
        , headers =
            case context.jwtToken of
                Just token ->
                    [ Http.header "Authorization" ("Bearer " ++ token) ]

                Nothing ->
                    []
        , url = context.apiBaseUrl ++ "/" ++ url
        , body = body
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


getRequest : S.Context -> Endpoint -> Json.Decoder a -> Http.Request a
getRequest context (Endpoint endpoint) decoder =
    request context "GET" endpoint Http.emptyBody (Http.expectJson decoder)


postRequest : S.Context -> Endpoint -> Http.Body -> Json.Decoder a -> Http.Request a
postRequest context (Endpoint endpoint) body decoder =
    request context "POST" endpoint body (Http.expectJson decoder)


patchRequest : S.Context -> String -> Http.Body -> Json.Decoder a -> Http.Request a
patchRequest context url body decoder =
    request context "PATCH" url body (Http.expectJson decoder)


deleteRequest : S.Context -> String -> Http.Request String
deleteRequest context url =
    request context "DELETE" url Http.emptyBody Http.expectString



-- SPECIAL REQUEST TYPES


getRequestString : S.Context -> String -> Http.Request String
getRequestString context url =
    request context "GET" url Http.emptyBody Http.expectString


postRequestEmpty : S.Context -> String -> Http.Request String
postRequestEmpty context url =
    request context "POST" url Http.emptyBody Http.expectString


patchRequestEmptyResponse : S.Context -> String -> Http.Body -> Http.Request String
patchRequestEmptyResponse context url body =
    request context "PATCH" url body Http.expectString
