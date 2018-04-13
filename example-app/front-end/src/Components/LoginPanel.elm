module Components.LoginPanel exposing (..)

import Bulma.Elements exposing (box)
import Form exposing (Form)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Ports exposing (saveToken)
import RemoteData as RD exposing (WebData)
import Server.Api.AuthAPI exposing (performLogin)
import Server.Config
import Types.Login exposing (LoginForm, LoginResponse)
import Views.LoginPanel as LPV


-- MODEL


type alias Model =
    { formModel : Form () LoginForm
    , serverContext : Server.Config.Context
    }


init : Server.Config.Context -> Model
init serverContext =
    { formModel = Form.initial [] LPV.validation
    , serverContext = serverContext
    }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | ReceiveLogin (WebData LoginResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveLogin remoteResult ->
            case remoteResult of
                RD.Success { jwtToken } ->
                    ( model, saveToken jwtToken )

                _ ->
                    ( model, Cmd.none )

        FormMsg formMsg ->
            let
                updatedFormModel =
                    Form.update LPV.validation formMsg model.formModel

                modelWithUpdatedForm =
                    { model | formModel = updatedFormModel }
            in
            case formMsg of
                Form.Submit ->
                    let
                        { formModel, serverContext } =
                            model

                        submitCmd =
                            case Form.getOutput formModel of
                                Just fModel ->
                                    Cmd.map ReceiveLogin <| performLogin fModel serverContext

                                Nothing ->
                                    Cmd.none
                    in
                    ( modelWithUpdatedForm, submitCmd )

                _ ->
                    ( modelWithUpdatedForm
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view { formModel } =
    box [ style [ ( "display", "inline-flex" ) ] ] [ Html.map FormMsg <| LPV.view formModel ]
