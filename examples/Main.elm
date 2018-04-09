module Main exposing (main)

import Html as H exposing (Html, program)
import Html.Attributes as A
import Html.Events as E exposing (on)
import Json.Decode as Decode exposing (Decoder)
import RangeSlider


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model --


type alias Model =
    { currentLinearRange : ( Float, Float )
    , linearRangeSliderState : RangeSlider.State
    , prop : Float
    }


init : ( Model, Cmd msg )
init =
    { linearRangeSliderState = RangeSlider.initialState
    , currentLinearRange = ( 10.0, 50.0 )
    , prop = -1.0
    }
        ! []



-- RangeSlider --


linearRangeConfig : RangeSlider.Config Msg Float
linearRangeConfig =
    RangeSlider.simpleConfig
        { minValue = 0
        , maxValue = 100.0
        , tickCount = Just 10
        , format = toString
        , step = Just 5
        , onChangeLower = Just UpdateRangeLower
        , onChangeUpper = Just UpdateRangeUpper
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map RangeMsg <|
            RangeSlider.subscriptions model.linearRangeSliderState
        ]



-- Update --


type Msg
    = UpdateRangeLower Float
    | UpdateRangeUpper Float
    | RangeMsg RangeSlider.Msg
    | Info Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Info prop ->
            { model | prop = prop } ! []

        UpdateRangeUpper val ->
            let
                ( oldLower, oldUpper ) =
                    model.currentLinearRange

                newRange =
                    ( oldLower, max oldLower val )
            in
                { model | currentLinearRange = newRange } ! []

        UpdateRangeLower val ->
            let
                ( oldLower, oldUpper ) =
                    model.currentLinearRange

                newRange =
                    ( min oldUpper val, oldUpper )
            in
                { model | currentLinearRange = newRange } ! []

        RangeMsg subMsg ->
            let
                ( newState, maybeMsg ) =
                    RangeSlider.update
                        linearRangeConfig
                        subMsg
                        model.linearRangeSliderState

                -- model.currentLinearRange
                newModel =
                    { model | linearRangeSliderState = newState }
            in
                case maybeMsg of
                    Just updateMsg ->
                        update updateMsg newModel

                    _ ->
                        newModel ! []

        _ ->
            model ! []



-- View --


view : Model -> Html Msg
view model =
    H.div [ A.class "flex-container" ]
        [ H.div
            [ A.style [ ( "width", "50%" ) ]
            ]
            [ viewRange
                linearRangeConfig
                model.linearRangeSliderState
                model.currentLinearRange
            ]
        ]


onThing : (Float -> Msg) -> H.Attribute Msg
onThing msg =
    on "click" <|
        Decode.map3 (\width parentLeft childLeft -> msg <| (childLeft - parentLeft) / width)
            -- (parentElement offsetLeft)
            (target <| parentElement offsetWidth)
            (target <| parentElement offsetLeft)
            (target offsetLeft)


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


viewRange :
    RangeSlider.Config msg Float
    -> RangeSlider.State
    -> ( Float, Float )
    -> Html Msg
viewRange config state currentRange =
    H.map (RangeMsg) <|
        RangeSlider.viewUnstyled
            RangeSlider.defaultStyle
            config
            state
            currentRange
