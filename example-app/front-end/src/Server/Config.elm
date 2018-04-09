module Server.Config exposing (..)


type alias Context =
    { apiBaseUrl : String
    , jwtToken : Maybe String
    }


apiUrl : Context -> Endpoint -> String
apiUrl { apiBaseUrl } endpoint =
    apiBaseUrl ++ "/" ++ endpoint


type alias Endpoint =
    String
