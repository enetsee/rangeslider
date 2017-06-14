module Ticks exposing (ticks)

{-| Nice ticks for linear scales
-}

{- Generate a `nice` set of intervals given a start and stop value and an
   approximate number of points. Note, the returned list may not have the number
   of intervals specified
-}


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
