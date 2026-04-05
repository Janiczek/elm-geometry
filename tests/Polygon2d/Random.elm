module Polygon2d.Random exposing (polygon2d)

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import Fuzz exposing (Fuzzer)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity
import Vector2d


radialPolygonWithHole : BoundingBox2d units coordinates -> Fuzzer (Polygon2d units coordinates)
radialPolygonWithHole boundingBox =
    let
        centerPoint =
            BoundingBox2d.centerPoint boundingBox

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        minRadius =
            Quantity.multiplyBy 0.05 (Quantity.min width height)

        maxRadius =
            Quantity.multiplyBy 0.5 (Quantity.min width height)
                |> Quantity.minus minRadius

        midRadius =
            Quantity.midpoint minRadius maxRadius

        innerRadiusFuzzer =
            Fuzz.map
                (Quantity.interpolateFrom
                    minRadius
                    (Quantity.multiplyBy 0.95 midRadius)
                )
                (Fuzz.floatRange 0 1)

        outerRadiusFuzzer =
            Fuzz.map
                (Quantity.interpolateFrom
                    (Quantity.multiplyBy 1.05 midRadius)
                    maxRadius
                )
                (Fuzz.floatRange 0 1)
    in
    Fuzz.intRange 3 32
        |> Fuzz.andThen
            (\numPoints ->
                Fuzz.listOfLength numPoints
                    (Fuzz.pair innerRadiusFuzzer outerRadiusFuzzer)
                    |> Fuzz.map
                        (List.indexedMap
                            (\index ( innerRadius, outerRadius ) ->
                                let
                                    angleFraction =
                                        toFloat index / toFloat numPoints

                                    angle =
                                        Quantity.multiplyBy angleFraction
                                            (Angle.turns 1)

                                    innerRadialVector =
                                        Vector2d.rTheta innerRadius angle

                                    outerRadialVector =
                                        Vector2d.rTheta outerRadius angle

                                    innerPoint =
                                        centerPoint |> Point2d.translateBy innerRadialVector

                                    outerPoint =
                                        centerPoint |> Point2d.translateBy outerRadialVector
                                in
                                ( innerPoint, outerPoint )
                            )
                        )
                    |> Fuzz.map List.unzip
                    |> Fuzz.map
                        (\( innerLoop, outerLoop ) ->
                            Polygon2d.withHoles [ List.reverse innerLoop ] outerLoop
                        )
            )


type alias GridPolygon =
    { outerLoop : List ( Int, Int )
    , innerLoops : List (List ( Int, Int ))
    }


localCoordinates : Fuzzer ( Float, Float )
localCoordinates =
    Fuzz.pair (Fuzz.floatRange 0.1 0.9) (Fuzz.floatRange 0.1 0.9)


loopPoints : BoundingBox2d units coordinates -> List ( Int, Int ) -> Fuzzer (List (Point2d units coordinates))
loopPoints boundingBox gridCoordinates =
    let
        { minX, minY } =
            BoundingBox2d.extrema boundingBox

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        xStart =
            minX |> Quantity.plus (Quantity.multiplyBy 0.15 width)

        yStart =
            minY |> Quantity.plus (Quantity.multiplyBy 0.15 width)

        xStep =
            width |> Quantity.multiplyBy (0.7 / 8)

        yStep =
            height |> Quantity.multiplyBy (0.7 / 8)

        gridPoint ( i, j ) ( u, v ) =
            let
                px =
                    xStart |> Quantity.plus (xStep |> Quantity.multiplyBy (toFloat i + u))

                py =
                    yStart |> Quantity.plus (yStep |> Quantity.multiplyBy (toFloat j + v))
            in
            Point2d.xy px py
    in
    Fuzz.listOfLength (List.length gridCoordinates) localCoordinates
        |> Fuzz.map (List.map2 gridPoint gridCoordinates)


squareOuterLoop : List ( Int, Int )
squareOuterLoop =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( 4, 0 )
    , ( 5, 0 )
    , ( 6, 0 )
    , ( 7, 0 )
    , ( 7, 1 )
    , ( 7, 2 )
    , ( 7, 3 )
    , ( 7, 4 )
    , ( 7, 5 )
    , ( 7, 6 )
    , ( 7, 7 )
    , ( 6, 7 )
    , ( 5, 7 )
    , ( 4, 7 )
    , ( 3, 7 )
    , ( 2, 7 )
    , ( 1, 7 )
    , ( 0, 7 )
    , ( 0, 6 )
    , ( 0, 5 )
    , ( 0, 4 )
    , ( 0, 3 )
    , ( 0, 2 )
    , ( 0, 1 )
    ]


squarish : GridPolygon
squarish =
    { outerLoop = squareOuterLoop
    , innerLoops = []
    }


lShaped : GridPolygon
lShaped =
    { outerLoop =
        [ ( 0, 0 )
        , ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 4, 0 )
        , ( 5, 0 )
        , ( 6, 0 )
        , ( 7, 0 )
        , ( 7, 1 )
        , ( 7, 2 )
        , ( 7, 3 )
        , ( 7, 4 )
        , ( 7, 5 )
        , ( 7, 6 )
        , ( 7, 7 )
        , ( 6, 7 )
        , ( 5, 7 )
        , ( 4, 7 )
        , ( 4, 6 )
        , ( 4, 5 )
        , ( 4, 4 )
        , ( 4, 3 )
        , ( 3, 3 )
        , ( 2, 3 )
        , ( 1, 3 )
        , ( 0, 3 )
        , ( 0, 2 )
        , ( 0, 1 )
        ]
    , innerLoops = []
    }


squareWithHole : GridPolygon
squareWithHole =
    { outerLoop = squareOuterLoop
    , innerLoops =
        [ [ ( 2, 2 )
          , ( 3, 2 )
          , ( 4, 2 )
          , ( 5, 2 )
          , ( 5, 3 )
          , ( 5, 4 )
          , ( 5, 5 )
          , ( 4, 5 )
          , ( 3, 5 )
          , ( 2, 5 )
          , ( 2, 4 )
          , ( 2, 3 )
          ]
        ]
    }


squareWithTwoHoles : GridPolygon
squareWithTwoHoles =
    { outerLoop = squareOuterLoop
    , innerLoops =
        [ [ ( 1, 1 )
          , ( 1, 2 )
          , ( 2, 2 )
          , ( 3, 2 )
          , ( 4, 2 )
          , ( 5, 2 )
          , ( 5, 3 )
          , ( 5, 4 )
          , ( 5, 5 )
          , ( 5, 6 )
          , ( 6, 6 )
          , ( 6, 5 )
          , ( 6, 4 )
          , ( 6, 3 )
          , ( 6, 2 )
          , ( 6, 1 )
          , ( 5, 1 )
          , ( 4, 1 )
          , ( 3, 1 )
          , ( 2, 1 )
          ]
        , [ ( 1, 4 )
          , ( 1, 5 )
          , ( 1, 6 )
          , ( 2, 6 )
          , ( 3, 6 )
          , ( 3, 5 )
          , ( 3, 4 )
          , ( 2, 4 )
          ]
        ]
    }


interlocking : GridPolygon
interlocking =
    { outerLoop = squareOuterLoop
    , innerLoops =
        [ [ ( 1, 1 )
          , ( 1, 2 )
          , ( 2, 2 )
          , ( 3, 2 )
          , ( 3, 3 )
          , ( 3, 4 )
          , ( 4, 4 )
          , ( 4, 3 )
          , ( 4, 2 )
          , ( 5, 2 )
          , ( 6, 2 )
          , ( 6, 1 )
          , ( 5, 1 )
          , ( 4, 1 )
          , ( 3, 1 )
          , ( 2, 1 )
          ]
        , [ ( 1, 3 )
          , ( 1, 4 )
          , ( 1, 5 )
          , ( 1, 6 )
          , ( 2, 6 )
          , ( 3, 6 )
          , ( 4, 6 )
          , ( 5, 6 )
          , ( 6, 6 )
          , ( 6, 5 )
          , ( 6, 4 )
          , ( 6, 3 )
          , ( 5, 3 )
          , ( 5, 4 )
          , ( 5, 5 )
          , ( 4, 5 )
          , ( 3, 5 )
          , ( 2, 5 )
          , ( 2, 4 )
          , ( 2, 3 )
          ]
        ]
    }



gridPolygon : BoundingBox2d units coordinates -> GridPolygon -> Fuzzer (Polygon2d units coordinates)
gridPolygon boundingBox { outerLoop, innerLoops } =
    let
        outerLoopFuzzer =
            loopPoints boundingBox outerLoop

        innerLoopFuzzers =
            List.map (loopPoints boundingBox) innerLoops
    in
    Fuzz.map2 Polygon2d.withHoles
        (Fuzz.sequence innerLoopFuzzers)
        outerLoopFuzzer


polygon2d : BoundingBox2d units coordinates -> Fuzzer (Polygon2d units coordinates)
polygon2d boundingBox =
    Fuzz.map2
        (\polygon angle ->
            polygon
                |> Polygon2d.rotateAround
                    (BoundingBox2d.centerPoint boundingBox)
                    angle
        )
        (Fuzz.oneOf
            [ radialPolygonWithHole boundingBox
            , gridPolygon boundingBox squarish
            , gridPolygon boundingBox lShaped
            , gridPolygon boundingBox squareWithHole
            , gridPolygon boundingBox squareWithTwoHoles
            , gridPolygon boundingBox interlocking
            ]
        )
        (Fuzz.map Angle.radians (Fuzz.floatRange -pi pi))
