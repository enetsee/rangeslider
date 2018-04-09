module RangeSlider.Linear
    exposing
        ( scale
        , unscale
        , interpolate
        , roundTo
        , deinterpolate
        , ticks
        )


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


interpolate : number -> number -> number -> number
interpolate a b =
    if a == b then
        \_ -> a
    else
        let
            delta =
                b - a
        in
            \t ->
                a + delta * t


roundTo : Float -> (a -> Float) -> a -> Float
roundTo step interpolate =
    (\c -> step * (toFloat << round <| c / step))
        << interpolate


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



{--Generate a `nice` set of intervals given a start and stop value and an
   approximate number of points. Note, the returned list may not have the number
   of intervals specified
--}


ticks : Float -> Float -> Int -> List Float
ticks start stop count =
    let
        reverse =
            if stop < start then
                List.reverse
            else
                identity

        step =
            tickIncrement start stop count
    in
        if step == 0.0 || isInfinite step then
            []
        else if step > 0.0 then
            let
                newStart =
                    ceiling (start / stop)

                newStop =
                    floor (stop / step)

                n =
                    newStop - newStart + 1
            in
                reverse <|
                    List.map (\i -> (toFloat <| newStart + i) * step) <|
                        List.range 0 (n - 1)
        else
            let
                newStart =
                    floor (start * step)

                newStop =
                    ceiling (stop * step)

                n =
                    newStart - newStop + 1
            in
                reverse <|
                    List.map (\i -> (toFloat <| newStart - i) / step) <|
                        List.range 0 (n - 1)


tickIncrement : Float -> Float -> Int -> Float
tickIncrement start stop count =
    let
        n =
            toFloat count

        step =
            (stop - start) / max 0 n

        power =
            toFloat <| floor <| log10 step

        error =
            step / (10 ^ power)

        m =
            if error >= e10 then
                10
            else if error >= e5 then
                5
            else if error >= e2 then
                2
            else
                1
    in
        if power >= 0 then
            m * (10 ^ power)
        else
            (-(10 ^ -power)) / m


ln10 : Float
ln10 =
    logBase e 10.0


log10 : Float -> Float
log10 =
    logBase 10.0


e10 : Float
e10 =
    sqrt 50.0


e5 : Float
e5 =
    sqrt 10.0


e2 : Float
e2 =
    sqrt 2.0



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
