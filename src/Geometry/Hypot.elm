--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Hypot exposing (hypot2, hypot3)

{-| `Math.hypot` semantics for two and three arguments, aiming for being more
precise than `sqrt (x*x + y*y)` (see eg. the tool Herbie).

<https://tc39.es/ecma262/multipage/numbers-and-dates.html#sec-math.hypot>
<https://github.com/v8/v8/blob/main/src/builtins/math.tq> (`FastMathHypot`)

-}


hypot2 : Float -> Float -> Float
hypot2 x y =
    if isInfinite x || isInfinite y then
        1 / 0

    else if isNaN x || isNaN y then
        0 / 0

    else if x == 0 && y == 0 then
        0

    else
        let
            a =
                abs x

            b =
                abs y

            m =
                max a b

            ra =
                a / m

            rb =
                b / m
        in
        sqrt (ra * ra + rb * rb) * m


hypot3 : Float -> Float -> Float -> Float
hypot3 x y z =
    if isInfinite x || isInfinite y || isInfinite z then
        1 / 0

    else if isNaN x || isNaN y || isNaN z then
        0 / 0

    else if x == 0 && y == 0 && z == 0 then
        0

    else
        let
            a =
                abs x

            b =
                abs y

            c =
                abs z

            m =
                max a (max b c)

            powerA =
                (a / m) * (a / m)

            powerB =
                (b / m) * (b / m)

            compensation =
                (powerA + powerB) - powerA - powerB

            powerC =
                (c / m) * (c / m) - compensation
        in
        sqrt (powerA + powerB + powerC) * m
