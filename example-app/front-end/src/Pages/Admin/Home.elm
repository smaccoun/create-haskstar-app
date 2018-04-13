module Pages.Admin.Home exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Html exposing (Html, div, h1, text)
import RemoteData exposing (RemoteData(..), WebData)
import Server.Api.Index exposing (userApiResourceParams)
import Server.Config
import Server.ResourceAPI exposing (getContainer)
import Types.User exposing (User)


init : Server.Config.Context -> ( Model, Cmd Msg )
init context =
    ( { users = NotAsked }
    , Cmd.map GetUsers <| getContainer (userApiResourceParams context)
    )


type alias Model =
    { users : WebData (List User) }


type Msg
    = GetUsers (WebData (List User))


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        GetUsers remoteUsers ->
            { model | users = remoteUsers } ! []


view : Model -> Html Msg
view model =
    columns columnsModifiers
        []
        [ column columnModifiers [] [ viewMenu ]
        , column columnModifiers [] [ viewRemoteUsers model.users ]
        ]


myNavbarBurger : Html Msg
myNavbarBurger =
    navbarBurger True
        []
        [ div [] []
        , div [] []
        , div [] []
        ]


viewMenu =
    menu []
        [ menuLabel [] [ text "General" ]
        , menuList []
            [ menuListItemLink False [] [ text "Dashboard" ]
            ]
        ]


viewRemoteUsers : WebData (List User) -> Html msg
viewRemoteUsers remoteUsers =
    case remoteUsers of
        Success users ->
            div [] [ viewUsers users ]

        _ ->
            div [] [ text "..." ]


viewUsers : List User -> Html msg
viewUsers users =
    div [] <|
        h1 [] [ text "Users" ]
            :: List.map viewUserRow users


viewUserRow : User -> Html msg
viewUserRow user =
    div [] [ text user.email ]
