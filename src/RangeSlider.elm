module RangeSlider
    exposing
        ( view
        , viewUnstyled
        , Config
        , config
        , StyleConfig
        , defaultStyle
        , simpleConfig
        , State
        , initialState
        , subscriptions
        , update
        , Msg
        )

{-| Range-slider input

# State
@docs State, initialState

# Configuration
@docs Config, config , simpleConfig

# Styles
@docs StyleConfig , defaultStyle
# View
@docs view, viewUnstyled

# Update
@docs update, Msg


# Subscriptions
@docs subscriptions
-}

import Html as HtmlUnstyled
import Html.Styled as H exposing (Html, Attribute)
import Html.Styled.Attributes as A
import Mouse exposing (Position)
import RangeSlider.Events as E
import RangeSlider.Helpers exposing (maybe, mkPx)
import RangeSlider.Linear as Linear
import RangeSlider.Style as Style


{-| Configuration of the range slider

- `domain` &mdash; the domain you wish to map from.
- `range` &mdash; the range you wish to map on to.
- `scale` &mdash; a mapping from domain to position
- `unscale` &mdash; a mapping from position to domain
- `ticks` &mdash; a list of values in domain-space to render on your rangeslider axis.
- `format` &mdash; a function to render values from your domain.
- `toMsg` &mdash; a way to send the new value of an endpoint to your app as messages.

-}
type Config msg domain
    = Config
        { domain : ( domain, domain )
        , scale : domain -> Maybe Float
        , unscale : Float -> Maybe domain
        , ticks : List domain
        , format : domain -> String
        , onChangeLower : Maybe (domain -> msg)
        , onChangeUpper : Maybe (domain -> msg)
        }


{-| A configuration of the range slider allowing you to specify the
type of your domain and the type of scale you wish to use. Note the
type of the range is fixed as `Float` as the scale must be invertable.
-}
config :
    { a
        | domain : ( domain, domain )
        , format : domain -> String
        , onChangeLower : Maybe (domain -> msg)
        , onChangeUpper : Maybe (domain -> msg)
        , scale : domain -> Maybe Float
        , ticks : List domain
        , unscale : Float -> Maybe domain
    }
    -> Config msg domain
config { domain, scale, unscale, ticks, format, onChangeLower, onChangeUpper } =
    Config
        { domain = domain
        , scale = scale
        , unscale = unscale
        , ticks = ticks
        , format = format
        , onChangeLower = onChangeLower
        , onChangeUpper = onChangeUpper
        }


{-| A simple configuration for the most common use case of a linear scale over
a floating point domain, with a range starting at 0. This configuration will
automatically calculate some visually `nice` ticks for you given an approximate
number of ticks you want.

You provide the following information in your table configuration:

- `domain` &mdash; the lower and upper endpoint of your range.
- `width` &mdash; the width of the rangeslider in px.
- `maybeRoundTo` &mdash; an optional value to which each endpoint's value should be rounded.
- `maybeTickCount` &mdash; an optional approximate tick count. Providing a value of `Nothing` will result in an axis with no tick marks.
- `format` &mdash; a function to render values from your domain.
- `toMsg` &mdash; a way to send the new value of an endpoint to your app as messages.

-}
simpleConfig :
    { a
        | format : Float -> String
        , maxValue : Float
        , minValue : Float
        , onChangeLower : Maybe (Float -> msg)
        , onChangeUpper : Maybe (Float -> msg)
        , step : Maybe Float
        , tickCount : Maybe Int
    }
    -> Config msg Float
simpleConfig { minValue, maxValue, tickCount, format, step, onChangeUpper, onChangeLower } =
    let
        scale =
            Linear.scale
                domain
                ( 0.0, 1.0 )
                True
                Linear.deinterpolate
                Linear.interpolate
                >> Just

        unscale =
            Linear.unscale
                domain
                ( 0.0, 1.0 )
                True
                interp
                >> Just

        interp =
            case step of
                Just r ->
                    \a b ->
                        Linear.roundTo r <|
                            Linear.interpolate a b

                _ ->
                    Linear.interpolate

        ticks =
            maybe [] (Linear.ticks minValue maxValue) tickCount

        domain =
            ( minValue, maxValue )
    in
        Config
            { domain = domain
            , scale = scale
            , unscale = unscale
            , ticks = ticks
            , format = format
            , onChangeUpper = onChangeUpper
            , onChangeLower = onChangeLower
            }


{-| -}
type alias StyleConfig =
    Style.Config


{-| -}
defaultStyle : Style.Config
defaultStyle =
    Style.default


{-| Tracks the drag state, if any, of an endpoint and
    the screenspace x-offset of the rangeslider handle control
-}
type State
    = Dragging { endpoint : Endpoint, bound : Float, current : Position, offsetX : Float, width : Float }
    | NotDragging


{-| Upper and lower endpoints of the range
-}
type Endpoint
    = Upper
    | Lower


{-| Initial, non-dragging, state.
-}
initialState : State
initialState =
    NotDragging



-- Update ----------------------------------------------------------------------


{-| -}
type Msg
    = DragStart Endpoint Float Position Float Float
    | DragAt Position
    | DragEnd Position
    | Set Endpoint Float


{-| -}
update :
    Config msg comparableDomain
    -> Msg
    -> State
    -> ( State, Maybe msg )
update (Config config) msg state =
    case ( msg, state ) of
        ( Set Upper value, _ ) ->
            ( state
            , Maybe.andThen
                (\handler ->
                    value
                        |> config.unscale
                        |> Maybe.map handler
                )
                config.onChangeUpper
            )

        ( Set Lower value, _ ) ->
            ( state
            , Maybe.andThen
                (\handler ->
                    value
                        |> config.unscale
                        |> Maybe.map handler
                )
                config.onChangeLower
            )

        ( DragStart endpoint bound position offset width, _ ) ->
            ( dragging endpoint bound position offset width
            , Nothing
            )

        ( DragAt position, Dragging { endpoint, bound, current, offsetX, width } ) ->
            ( dragging endpoint bound position offsetX width
            , Nothing
            )

        ( DragAt position, NotDragging ) ->
            ( NotDragging
            , Nothing
            )

        ( DragEnd position, Dragging drag ) ->
            let
                msg =
                    case drag.endpoint of
                        Upper ->
                            Maybe.andThen (\handler -> Maybe.map handler (dragEndHelper config drag))
                                config.onChangeUpper

                        Lower ->
                            Maybe.andThen (\handler -> Maybe.map handler (dragEndHelper config drag))
                                config.onChangeLower
            in
                ( NotDragging
                , msg
                )

        ( DragEnd _, NotDragging ) ->
            ( NotDragging
            , Nothing
            )



-- View ------------------------------------------------------------------------


{-| -}
viewUnstyled :
    StyleConfig
    -> Config msg Float
    -> State
    -> ( Float, Float )
    -> HtmlUnstyled.Html Msg
viewUnstyled styles config state range =
    view styles config state range
        |> H.toUnstyled


{-| -}
view : StyleConfig -> Config msg Float -> State -> ( Float, Float ) -> Html Msg
view styles (Config config) state ( lower, upper ) =
    let
        ( tempLower, tempUpper ) =
            draggingHelper config ( lower, upper ) state

        ( positionLower, positionUpper ) =
            ( config.scale tempLower |> Maybe.withDefault lower
            , config.scale tempUpper |> Maybe.withDefault upper
            )

        progressWidth =
            mkPx <| positionUpper - positionLower

        ( lowerLeft, upperLeft ) =
            ( mkPx positionLower, mkPx positionUpper )
    in
        H.div [ A.css styles.rangeSlider ]
            -- A.class "c-range-slider" ]
            [ H.div
                [ A.css styles.track
                  -- A.class "c-range-slider__track"
                ]
                []
            , H.div
                [ A.css styles.progress
                  -- A.class "c-range-slider__progress"
                , A.style [ ( "left", lowerLeft ), ( "width", progressWidth ) ]
                , E.onClick (onClickHelper positionLower positionUpper)
                ]
                []
            , H.div [] <|
                List.filterMap (viewTick styles config.scale config.format) config.ticks
              --A.class "c-range-slider__axis"
            , viewSliderHandle styles
                Lower
                (config.scale upper |> Maybe.withDefault 1.0)
                lowerLeft
                [ A.css styles.labelLower
                  --A.class "c-range-slider__label" ]
                ]
                [ H.text <| config.format tempLower ]
            , viewSliderHandle styles
                Upper
                (config.scale lower |> Maybe.withDefault 0.0)
                upperLeft
                [ A.css styles.labelUpper
                  --A.class "c-range-slider__label" ]
                ]
                [ H.text <| config.format tempUpper ]
            ]


viewTick : StyleConfig -> (a -> Maybe Float) -> (a -> String) -> a -> Maybe (Html msg)
viewTick styles scale format tick =
    scale tick
        |> Maybe.map
            (\x ->
                let
                    left =
                        mkPx x
                in
                    H.div
                        [ A.css styles.tick
                          -- A.class "c-range-slider__tick"
                        , A.style [ ( "left", left ) ]
                        ]
                        [ H.text <| format tick ]
            )


viewSliderHandle :
    StyleConfig
    -> Endpoint
    -> Float
    -> String
    -> List (Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
viewSliderHandle styles endpoint bound left attrs content =
    let
        style =
            case endpoint of
                Lower ->
                    styles.handleLower

                Upper ->
                    styles.handleUpper
    in
        H.span
            [ E.onMouseDown (DragStart endpoint bound)
              -- , A.class "c-range-slider__handle"
            , A.css style
            , A.style [ ( "left", left ) ]
            , A.tabindex 0
            ]
            [ H.span attrs content ]



-- Subscriptions ---------------------------------------------------------------


{-| Subscriptions that should be added to your app's own subscriptions.
-}
subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        NotDragging ->
            Sub.none

        _ ->
            Sub.batch
                [ Mouse.moves DragAt
                , Mouse.ups DragEnd
                ]



-- Helpers ---------------------------------------------------------------------


dragging : Endpoint -> Float -> Position -> Float -> Float -> State
dragging endpoint bound current offsetX width =
    Dragging
        { endpoint = endpoint
        , bound = bound
        , current = current
        , offsetX = offsetX
        , width = width
        }


dragEndHelper :
    { b | unscale : Float -> a }
    -> { d
        | bound : Float
        , current : { c | x : Int }
        , endpoint : Endpoint
        , offsetX : Float
        , width : Float
       }
    -> a
dragEndHelper { unscale } { endpoint, bound, current, offsetX, width } =
    let
        ( l, u ) =
            case endpoint of
                Lower ->
                    ( 0.0, bound )

                Upper ->
                    ( bound, 1.0 )
    in
        unscale <|
            clamp l u <|
                toFloat (current.x - floor offsetX)
                    / width


draggingHelper :
    { b | unscale : Float -> Maybe a }
    -> ( a, a )
    -> State
    -> ( a, a )
draggingHelper { unscale } ( lower, upper ) state =
    case state of
        Dragging { endpoint, bound, current, offsetX, width } ->
            let
                x =
                    (toFloat current.x - offsetX) / width
            in
                case endpoint of
                    Lower ->
                        ( clamp 0 bound x |> unscale |> Maybe.withDefault lower
                        , upper
                        )

                    Upper ->
                        ( lower
                        , clamp bound 1.0 x |> unscale |> Maybe.withDefault upper
                        )

        _ ->
            ( lower, upper )


onClickHelper : Float -> Float -> { a | x : Int } -> Float -> Float -> Msg
onClickHelper lower upper current offsetX width =
    let
        prop =
            clamp lower upper <|
                toFloat (current.x - floor offsetX)
                    / width
    in
        if (abs <| prop - lower) < (abs <| upper - prop) then
            Set Lower <| prop
        else
            Set Upper <| prop
