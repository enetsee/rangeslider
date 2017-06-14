module Linear exposing (scale, unscale, deinterpolate)

{-| A simple linear scale for the usual range-slider use case -
-}


scale :
    ( comparable, comparable )
    -> ( d, d )
    -> Bool
    -> (comparable -> comparable -> comparable -> number)
    -> (d -> d -> number -> a)
    -> comparable
    -> a
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
            deinterpolate
            r
            y


deinterpolate : Float -> Float -> Float -> Float
deinterpolate a b =
    let
        delta =
            b - a
    in
        if a == b then
            \_ -> delta
        else
            \x -> (x - a) / delta



-- Scale helpers ---------------------------------------------------------------


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
