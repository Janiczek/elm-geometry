module Tests.Triangle2d exposing (triangleContainsOwnCentroid, triangleAreaNonnegative)

import Expect
import Geometry.FuzzTest as Test
import Geometry.Random as Random
import Quantity
import Test exposing (Test)
import Triangle2d


triangleContainsOwnCentroid : Test
triangleContainsOwnCentroid =
    Test.check "non-zero area triangle contains its own centroid"
        Random.triangle2d
        (\triangle ->
            let
                centroid =
                    Triangle2d.centroid triangle

                area =
                    Triangle2d.area triangle
            in
            if area == Quantity.zero || Triangle2d.contains centroid triangle then
                Expect.pass

            else
                Expect.fail "non-zero area triangle did not contain its own centroid"
        )


triangleAreaNonnegative : Test
triangleAreaNonnegative =
    Test.check "Triangle2d.area is nonnegative"
        Random.triangle2d
        (\triangle ->
            let
                area =
                    Triangle2d.area triangle
            in
            if area |> Quantity.greaterThanOrEqualTo Quantity.zero then
                Expect.pass

            else
                Expect.fail "Triangle2d.area was negative"
        )
