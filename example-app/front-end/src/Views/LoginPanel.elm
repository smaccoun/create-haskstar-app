module Views.LoginPanel exposing (..)

import Bulma.Elements as E
import Bulma.Form as BForm exposing (controlInput, controlInputModifiers, controlPassword, controlText)
import Bulma.Layout exposing (container)
import Form exposing (Form)
import Form.Input as Input exposing (Input)
import Form.Validate as Validate exposing (..)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias LoginForm =
    { email : String
    , password : String
    }


validation : Validation () LoginForm
validation =
    map2 LoginForm
        (field "email" email)
        (field "password" string)


view : Form () LoginForm -> Html Form.Msg
view form =
    container [ style [ ( "width", "300px" ) ] ]
        [ viewInputField "Email" form
        , viewInputField "Password" form
        , E.button E.buttonModifiers
            [ onClick Form.Submit ]
            [ text "Submit" ]
        ]


viewInputField : String -> Form () LoginForm -> Html Form.Msg
viewInputField labelValue form =
    let
        fieldId =
            String.toLower labelValue

        fieldValue =
            Form.getFieldAsString fieldId form
    in
    BForm.field []
        [ BForm.controlLabel [] [ text labelValue ]
        , if labelValue == "Email" then
            Input.textInput (Form.getFieldAsString fieldId form) []
          else
            Input.passwordInput (Form.getFieldAsString fieldId form) []
        , errorFor fieldValue
        ]


errorFor field =
    case field.liveError of
        Just error ->
            -- replace toString with your own translations
            div [ style [ ( "color", "red" ) ] ] [ text (toString error) ]

        Nothing ->
            text ""
