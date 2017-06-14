module Main exposing (main)

import Html as H exposing (Html, program)
import Html.Attributes as A
import RangeSlider exposing (Endpoint(..))


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
    , currentLogRange : ( Float, Float )
    , linearRangeSliderState : RangeSlider.State
    , logRangeSliderState : RangeSlider.State
    }


init : ( Model, Cmd msg )
init =
    { linearRangeSliderState = RangeSlider.initialState
    , currentLinearRange = ( 10.0, 50.0 )
    , logRangeSliderState = RangeSlider.initialState
    , currentLogRange = ( 100000.0, 400000.0 )
    }
        ! []



-- RangeSlider --


type WhichRange
    = Linear
    | Log


linearRangeConfig : RangeSlider.Config Msg Float
linearRangeConfig =
    RangeSlider.simpleConfig
        { domain = ( 0.0, 100.0 )
        , width = 300.0
        , maybeTickCount = Just 5
        , format = toString
        , maybeRoundTo = Just 5
        , toMsg = (\endpoint val -> Just <| UpdateRange Linear endpoint val)
        }


logRangeConfig : RangeSlider.Config Msg Float
logRangeConfig =
    let
        domain =
            ( 10000.0, 1000000.0 )

        width =
            300.0

        range =
            ( 0.0, width )

        scale =
            logScale domain width

        unscale =
            logUnscale domain width (Just 10000)

        ticks =
            [ 10000, 100000, 1000000 ]

        isRound x =
            (abs <| x - (toFloat <| round x)) < 1.0e-6

        format x =
            let
                p =
                    logBase 10 x
            in
                if isRound p then
                    "10e" ++ toString (round p)
                else
                    toString x
    in
        RangeSlider.config
            { domain = domain
            , range = range
            , scale = scale
            , unscale = unscale
            , format = format
            , ticks = ticks
            , toMsg = (\endpoint val -> Just <| UpdateRange Log endpoint val)
            }



-- Custom log scale & ticks  --


logScale : ( Float, Float ) -> Float -> Float -> Float
logScale domain width =
    scale
        domain
        ( 0.0, width )
        True
        deinterpolateLog
        interpolateLinear


logUnscale : ( Float, Float ) -> Float -> Maybe Float -> Float -> Float
logUnscale domain width roundTo =
    unscale
        domain
        ( 0.0, width )
        True
        (case roundTo of
            Just r ->
                \a b ->
                    interpolateRoundTo r <| reinterpolateLog a b

            _ ->
                reinterpolateLog
        )


interpolateLinear : number -> number -> number -> number
interpolateLinear a b =
    let
        delta =
            b - a
    in
        \t ->
            a + delta * t


deinterpolateLinear : Float -> Float -> Float -> Float
deinterpolateLinear a b =
    if a == b then
        Basics.always <| b - a
    else
        let
            delta =
                b - a
        in
            \x -> (x - a) / delta


interpolateRoundTo : Float -> (a -> Float) -> a -> Float
interpolateRoundTo step interpolate =
    (\x -> step * toFloat (round (x / step)))
        << interpolate


clampWith :
    comparable
    -> comparable
    -> a
    -> a
    -> (comparable -> a)
    -> comparable
    -> a
clampWith mn mx a b f x =
    if x <= mn then
        a
    else if x >= mx then
        b
    else
        f x


deinterpolateClamp :
    (comparable -> comparable -> comparable -> number)
    -> comparable
    -> comparable
    -> comparable
    -> number
deinterpolateClamp deinterpolate a b =
    clampWith a b 0 1 <|
        deinterpolate a b


reinterpolateClamp : (a -> a -> number -> a) -> a -> a -> number -> a
reinterpolateClamp reinterpolate a b =
    clampWith 0 1 a b <|
        reinterpolate a b


bimap :
    ( comparable, comparable )
    -> ( d, d )
    -> (comparable -> comparable -> a -> b)
    -> (d -> d -> b -> c)
    -> a
    -> c
bimap ( d0, d1 ) ( r0, r1 ) deinterpolate reinterpolate =
    let
        ( d, r ) =
            if d1 < d0 then
                ( deinterpolate d1 d0, reinterpolate r1 r0 )
            else
                ( deinterpolate d0 d1, reinterpolate r0 r1 )
    in
        r << d


scale :
    ( comparable, comparable )
    -> ( a, a )
    -> Bool
    -> (comparable -> comparable -> comparable -> number)
    -> (a -> a -> number -> b)
    -> comparable
    -> b
scale domain range clamp deinterpolate interpolate x =
    let
        d =
            if clamp then
                deinterpolateClamp deinterpolate
            else
                deinterpolate
    in
        bimap
            domain
            range
            d
            interpolate
            x


unscale :
    ( a, a )
    -> ( Float, Float )
    -> Bool
    -> (a -> a -> Float -> a)
    -> Float
    -> a
unscale domain range clamp reinterpolate y =
    let
        r =
            if clamp then
                reinterpolateClamp reinterpolate
            else
                reinterpolate
    in
        bimap
            range
            domain
            deinterpolateLinear
            r
            y


log10 : Float -> Float
log10 =
    Basics.logBase 10


deinterpolateLog : Float -> Float -> Float -> Float
deinterpolateLog a b =
    let
        delta =
            log10 (b / a)
    in
        if delta == 0.0 then
            always delta
        else
            \x -> log10 (x / a) / delta


pow : number -> number -> number
pow x y =
    x ^ y


reinterpolateLog : Float -> Float -> Float -> Float
reinterpolateLog a b =
    if a < 0.0 then
        \t -> -(-b ^ t * -a ^ (1 - t))
    else
        \t -> b ^ t * a ^ (1 - t)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (RangeMsg Linear) <|
            RangeSlider.subscriptions model.linearRangeSliderState
        , Sub.map (RangeMsg Log) <|
            RangeSlider.subscriptions model.logRangeSliderState
        ]



-- Update --


type Msg
    = UpdateRange WhichRange RangeSlider.Endpoint Float
    | RangeMsg WhichRange RangeSlider.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRange Linear endpoint val ->
            let
                ( oldLower, oldUpper ) =
                    model.currentLinearRange

                newRange =
                    case endpoint of
                        Lower ->
                            ( val, oldUpper )

                        _ ->
                            ( oldLower, val )
            in
                { model | currentLinearRange = newRange } ! []

        UpdateRange Log endpoint val ->
            let
                ( oldLower, oldUpper ) =
                    model.currentLogRange

                newRange =
                    case endpoint of
                        Lower ->
                            ( val, oldUpper )

                        _ ->
                            ( oldLower, val )
            in
                { model | currentLogRange = newRange } ! []

        RangeMsg Linear subMsg ->
            let
                ( newState, maybeMsg ) =
                    RangeSlider.update
                        linearRangeConfig
                        subMsg
                        model.linearRangeSliderState
                        model.currentLinearRange

                newModel =
                    { model | linearRangeSliderState = newState }
            in
                case maybeMsg of
                    Just updateMsg ->
                        update updateMsg newModel

                    _ ->
                        newModel ! []

        RangeMsg Log subMsg ->
            let
                ( newState, maybeMsg ) =
                    RangeSlider.update
                        logRangeConfig
                        subMsg
                        model.logRangeSliderState
                        model.currentLogRange

                newModel =
                    { model | logRangeSliderState = newState }
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
        [ viewRange
            Linear
            linearRangeConfig
            model.linearRangeSliderState
            model.currentLinearRange
        , viewRange
            Log
            logRangeConfig
            model.logRangeSliderState
            model.currentLogRange
        ]


viewRange :
    WhichRange
    -> RangeSlider.Config msg comparable
    -> RangeSlider.State
    -> ( comparable, comparable )
    -> Html Msg
viewRange which config state currentRange =
    H.div []
        [ H.div [ A.class "row" ]
            [ H.div [ A.class "flex-item" ]
                [ H.map (RangeMsg which) <|
                    RangeSlider.view
                        config
                        state
                        currentRange
                ]
            ]
        ]
