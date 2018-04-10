module Link exposing (link, navigate)

import Html exposing (Attribute, Html, a, div, header, img, li, option, select, text, ul)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder)
import Navigation


navigate : model -> String -> ( model, Cmd msg )
navigate model destination =
    ( model, Navigation.newUrl destination )


link : msg -> Attribute msg
link message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (preventDefault2
            |> Json.Decode.andThen (maybePreventDefault message)
        )


preventDefault2 : Decoder Bool
preventDefault2 =
    Json.Decode.map2
        invertedOr
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


maybePreventDefault : msg -> Bool -> Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Json.Decode.succeed msg

        False ->
            Json.Decode.fail "Normal link"


invertedOr : Bool -> Bool -> Bool
invertedOr x y =
    not (x || y)
