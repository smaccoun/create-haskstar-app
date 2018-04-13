module Server.Config exposing (..)


type Endpoint
    = Endpoint String


type alias Context =
    { apiBaseUrl : String
    , jwtToken : Maybe String
    }


apiUrl : Context -> Endpoint -> String
apiUrl { apiBaseUrl } (Endpoint endpoint) =
    apiBaseUrl ++ "/" ++ endpoint
