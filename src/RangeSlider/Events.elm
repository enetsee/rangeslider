module RangeSlider.Events exposing (onMouseDown, onClick)

import Html.Styled.Events as E
import Mouse exposing (Position)
import Json.Decode as Decode exposing (Decoder)
import Html.Styled exposing (Attribute)
import RangeSlider.Helpers exposing (maybe)


onClick : (Position -> Float -> Float -> value) -> Attribute value
onClick toMsg =
    E.onWithOptions "click" eventOptions <|
        Decode.map3
            toMsg
            Mouse.position
            (target <| parentElement offsetLeft)
            (target <| parentElement offsetWidth)


onMouseDown : (Position -> Float -> Float -> value) -> Attribute value
onMouseDown toMsg =
    E.onWithOptions "mousedown" eventOptions <|
        Decode.map3
            toMsg
            Mouse.position
            (target <| parentElement offsetLeft)
            (target <| parentElement offsetWidth)


eventOptions : { preventDefault : Bool, stopPropagation : Bool }
eventOptions =
    { preventDefault = True
    , stopPropagation = True
    }



-- onTouchStartPreventDefault toMsg =
--     let
--         eventOptions =
--             { preventDefault = True
--             , stopPropagation = True
--             }
--     in
--         Html.Events.onWithOptions "touchstart" eventOptions (Json.Decode.succeed msg)


touches : Decoder a -> Decoder (List a)
touches decoder =
    let
        aux i xs =
            Decode.maybe (Decode.field (toString i) decoder)
                |> Decode.andThen
                    (maybe (Decode.succeed xs) (\x -> aux (i + 1) (x :: xs)))
    in
        Decode.at [ "touches", "0" ] <|
            aux 0 []


parentElement : Decoder a -> Decoder a
parentElement decoder =
    Decode.field "parentElement" decoder


offsetWidth : Decoder Float
offsetWidth =
    Decode.field "offsetWidth" Decode.float


target : Decoder a -> Decoder a
target decoder =
    Decode.field "target" decoder


offsetLeft : Decoder Float
offsetLeft =
    Decode.field "offsetLeft" Decode.float
