module RangeSlider
    exposing
        ( view
        , State
        , Endpoint(Lower, Upper)
        , initialState
        , Config
        , config
        , simpleConfig
        , subscriptions
        , update
        , Msg
        )

{-| Range-slider input

# State
@docs State, initialState, Endpoint

# View
@docs view

# Update
@docs update, Msg

# Configuration
@docs Config , config, simpleConfig

# Subscriptions
@docs subscriptions
-}

import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E
import Mouse exposing (Position)
import Json.Decode as Decode exposing (Decoder)
import Tuple exposing (first, second)
import Linear
import Interpolate
import Ticks


{-| Tracks the drag state, if any, of an endpoint and
    the screenspace x-offset of the rangeslider handle control
-}
type State
    = State (Maybe ( Endpoint, Drag, Float ))


{-| Initial, non-dragging, state.
-}
initialState : State
initialState =
    State Nothing


{-| Upper and lower endpoints of the range
-}
type Endpoint
    = Upper
    | Lower


{-| -}
type Msg
    = DragStart Endpoint Position
    | DragAt Position
    | DragEnd Position


type alias Drag =
    { start : Position
    , current : Position
    }



-- Configuration ---------------------------------------------------------------


{-| Configuration of the range slider
-}
type Config msg domain
    = Config
        { domain : ( domain, domain )
        , range : ( Float, Float )
        , scale : domain -> Float
        , unscale : Float -> domain
        , ticks : List domain
        , format : domain -> String
        , toMsg : Endpoint -> domain -> Maybe msg
        }


{-| A configuration of the range slider allowing you to specify the
type of your domain and the type of scale you wish to use. Note the
type of the range is fixed as `Float` as the scale must be invertable.

The two use cases for this configuration are
- When you want to have a rangeslider over some type other than `Float`, e.g.
`Date`.
- When your data is skewed and you require some non-linear scale for your input
e.g. a log scale.

You provide the following information in your table configuration:

- `domain` &mdash; the domain you wish to map from.
- `range` &mdash; the range you wish to map on to.
- `scale` &mdash; a mapping from domain to position
- `unscale` &mdash; a mapping from position to domain
- `ticks` &mdash; a list of values in domain-space to render on your rangeslider axis.
- `format` &mdash; a function to render values from your domain.
- `toMsg` &mdash; a way to send the new value of an endpoint to your app as messages.

-}
config :
    { a
        | domain : ( domain, domain )
        , range : ( Float, Float )
        , scale : domain -> Float
        , unscale : Float -> domain
        , ticks : List domain
        , format : domain -> String
        , toMsg : Endpoint -> domain -> Maybe msg
    }
    -> Config msg domain
config { domain, range, scale, unscale, ticks, format, toMsg } =
    Config
        { domain = domain
        , range = range
        , scale = scale
        , unscale = unscale
        , ticks = ticks
        , format = format
        , toMsg = toMsg
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
        | domain : ( Float, Float )
        , width : Float
        , maybeRoundTo : Maybe Float
        , maybeTickCount : Maybe Int
        , format : Float -> String
        , toMsg : Endpoint -> Float -> Maybe msg
    }
    -> Config msg Float
simpleConfig { domain, width, maybeTickCount, format, maybeRoundTo, toMsg } =
    let
        scale =
            Linear.scale
                domain
                range
                True
                Linear.deinterpolate
                Interpolate.linear

        unscale =
            Linear.unscale
                domain
                range
                True
                (case maybeRoundTo of
                    Just r ->
                        \a b ->
                            Interpolate.roundTo r <|
                                Interpolate.linear a b

                    _ ->
                        Interpolate.linear
                )

        ticks =
            maybe [] (Ticks.ticks d0 d1) maybeTickCount

        range =
            ( 0.0, width )

        ( d0, d1 ) =
            domain
    in
        Config
            { domain = domain
            , range = range
            , scale = scale
            , unscale = unscale
            , ticks = ticks
            , format = format
            , toMsg = toMsg
            }



-- Update ----------------------------------------------------------------------


{-| -}
update :
    Config msg comparableDomain
    -> Msg
    -> State
    -> ( comparableDomain, comparableDomain )
    -> ( State, Maybe msg )
update (Config config) msg (State state) ( lower, upper ) =
    case msg of
        DragStart endpoint position ->
            let
                ux =
                    case endpoint of
                        Upper ->
                            config.scale upper

                        Lower ->
                            config.scale lower
            in
                ( State <| Just ( endpoint, Drag position position, ux )
                , Nothing
                )

        DragAt position ->
            let
                newState =
                    Maybe.map
                        (\( endpoint, drag, ux ) ->
                            ( endpoint, { drag | current = position }, ux )
                        )
                        state
            in
                ( State newState, Nothing )

        DragEnd position ->
            case state of
                Just ( endpoint, { start }, offsetX ) ->
                    let
                        newValue =
                            absolutePosition
                                (config.unscale)
                                (clamp l u)
                                start
                                position
                                offsetX

                        ( l, u ) =
                            case endpoint of
                                Lower ->
                                    ( first config.domain, upper )

                                Upper ->
                                    ( lower, second config.domain )
                    in
                        ( State Nothing
                        , config.toMsg endpoint newValue
                        )

                _ ->
                    ( State Nothing, Nothing )



-- Subscriptions ---------------------------------------------------------------


{-| Subscriptions that should be added to your app's own subscriptions.
-}
subscriptions : State -> Sub Msg
subscriptions (State state) =
    case state of
        Nothing ->
            Sub.none

        _ ->
            Sub.batch
                [ Mouse.moves DragAt
                , Mouse.ups DragEnd
                ]



-- View ------------------------------------------------------------------------


{-| -}
view :
    Config msg comparableDomain
    -> State
    -> ( comparableDomain, comparableDomain )
    -> Html Msg
view (Config config) (State state) ( lower, upper ) =
    let
        ( tempLower, tempUpper ) =
            case state of
                Just ( Lower, { start, current }, offsetX ) ->
                    let
                        clampIt =
                            clamp (first config.domain) upper
                    in
                        ( absolutePosition config.unscale clampIt start current offsetX
                        , upper
                        )

                Just ( Upper, { start, current }, offsetX ) ->
                    let
                        clampIt =
                            clamp lower (second config.domain)
                    in
                        ( lower
                        , absolutePosition config.unscale clampIt start current offsetX
                        )

                _ ->
                    ( lower, upper )

        ( positionLower, positionUpper ) =
            ( config.scale tempLower, config.scale tempUpper )

        progressWidth =
            mkPx <| positionUpper - positionLower

        ( lowerLeft, upperLeft ) =
            ( mkPx positionLower, mkPx positionUpper )

        sliderWidth =
            mkPx <| (second config.range) - (first config.range)
    in
        H.div [ A.class "slider", A.class "range-slider", A.style [ ( "width", sliderWidth ) ] ]
            [ H.div
                [ A.class "slider-track"
                , A.style [ ( "width", sliderWidth ) ]
                ]
                []
            , H.div
                [ A.class "slider-progress"
                , A.style [ ( "left", lowerLeft ), ( "width", progressWidth ) ]
                ]
                []
            , H.div [ A.class "slider-axis" ] <|
                List.map (viewTick config.scale config.format) config.ticks
            , viewSliderHandle
                Lower
                lowerLeft
                [ A.class "slider-label" ]
                [ H.text <| config.format tempLower ]
            , viewSliderHandle
                Upper
                upperLeft
                [ A.class "slider-label" ]
                [ H.text <| config.format tempUpper ]
            ]


viewTick : (a -> b) -> (a -> String) -> a -> Html msg
viewTick scale format tick =
    let
        left =
            toString (scale tick) ++ "px"
    in
        H.div
            [ A.class "slider-label"
            , A.style [ ( "left", left ) ]
            ]
            [ H.text <| format tick ]


viewSliderHandle :
    Endpoint
    -> String
    -> List (Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
viewSliderHandle endpoint left attrs content =
    H.span
        [ onMouseDown endpoint
        , A.class "slider-handle"
        , A.style [ ( "left", left ) ]
        , A.tabindex 0
        ]
        [ H.span attrs content ]



-- Events ----------------------------------------------------------------------


onMouseDown : Endpoint -> Attribute Msg
onMouseDown endpoint =
    E.on "mousedown" <|
        Decode.map (DragStart endpoint) Mouse.position


onTouch : Endpoint -> Attribute Msg
onTouch endpoint =
    E.on "touchstart" <|
        Decode.map (DragStart endpoint) Mouse.position



-- Helpers ---------------------------------------------------------------------


mkPx : Float -> String
mkPx px =
    toString px ++ "px"


maybe : a -> (b -> a) -> Maybe b -> a
maybe whenNothing withJust maybeVal =
    case maybeVal of
        Just val ->
            withJust val

        _ ->
            whenNothing


absolutePosition :
    (Float -> a)
    -> (a -> b)
    -> Position
    -> Position
    -> Float
    -> b
absolutePosition unscale clampToRange start current offsetX =
    clampToRange <|
        unscale <|
            offsetX
                + (toFloat current.x)
                - (toFloat start.x)
