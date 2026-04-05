module Geometry.Random exposing
    ( parameterValue, scale, length, nonzeroLength, positiveLength, nonzeroPositiveLength, angle, unitlessQuantity, unitlessInterval
    , direction2d, direction3d, vector2d, vector3d, point2d, point3d
    , boundingBox2d, boundingBox3d, vectorBoundingBox2d, vectorBoundingBox3d
    , axis2d, axis3d, plane3d, sketchPlane3d, frame2d, frame3d
    , lineSegment2d, lineSegment3d, triangle2d, triangle3d, rectangle2d, nondegenerateRectangle2d, rectangle3d, polygon2d, block3d
    , arc2d, arc3d, circle2d, circle3d, ellipticalArc2d, ellipticalArc3d, ellipse2d
    , quadraticSpline2d, quadraticSpline3d, cubicSpline2d, cubicSpline3d, rationalQuadraticSpline2d, rationalQuadraticSpline3d, rationalCubicSpline2d, rationalCubicSpline3d, spline2d, spline3d
    , sphere3d, cylinder3d, cone3d, ellipsoid3d
    , smallList
    )

{-| Fuzzers for lots of different geometric types, intended for use in tests. 10 meters is
(arbitrarily) used as the basic scale for generated values, so for example the `point3d` fuzzer
will generate points where each coordinate value is in the range -10 meters to +10 meters.

This is only to have values be roughly the same scale (which helps to ensure that random tests
actually do trigger things like intersections between objects) but do not rely on geometry being
strictly contained within the -10...+10 meter bounds. For example, the `circle2d` fuzzer will
create circles where the _center_ point is within those bounds, and the radius is up to 10 meters,
meaning that the circle may well extend to larger or smaller coordinate values.

Most of the higher-level fuzzers in this module are built pretty directly from lower-level
fuzzers - for example the `axis3d` fuzzer is implemented as:

    axis3d : Fuzzer (Axis3d Meters coordinates)
    axis3d =
        Fuzz.map2 Axis3d.through point3d direction3d


# Scalars

@docs parameterValue, scale, length, nonzeroLength, positiveLength, nonzeroPositiveLength, angle, unitlessQuantity, unitlessInterval


# Primitives

@docs direction2d, direction3d, vector2d, vector3d, point2d, point3d


## Bounding boxes

@docs boundingBox2d, boundingBox3d, vectorBoundingBox2d, vectorBoundingBox3d


## Datums

@docs axis2d, axis3d, plane3d, sketchPlane3d, frame2d, frame3d


# Geometry

@docs lineSegment2d, lineSegment3d, triangle2d, triangle3d, rectangle2d, nondegenerateRectangle2d, rectangle3d, polygon2d, block3d


## Arcs, circles, ellipses

@docs arc2d, arc3d, circle2d, circle3d, ellipticalArc2d, ellipticalArc3d, ellipse2d


## Spline curves

@docs quadraticSpline2d, quadraticSpline3d, cubicSpline2d, cubicSpline3d, rationalQuadraticSpline2d, rationalQuadraticSpline3d, rationalCubicSpline2d, rationalCubicSpline3d, spline2d, spline3d


## 3D solids

@docs sphere3d, cylinder3d, cone3d, ellipsoid3d


# Utilities

@docs smallList

-}

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Cone3d exposing (Cone3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import Ellipsoid3d exposing (Ellipsoid3d)
import EllipticalArc2d exposing (EllipticalArc2d)
import EllipticalArc3d exposing (EllipticalArc3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polygon2d.Random
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import RationalCubicSpline2d exposing (RationalCubicSpline2d)
import RationalCubicSpline3d exposing (RationalCubicSpline3d)
import RationalQuadraticSpline2d exposing (RationalQuadraticSpline2d)
import RationalQuadraticSpline3d exposing (RationalQuadraticSpline3d)
import Rectangle2d exposing (Rectangle2d)
import Rectangle3d exposing (Rectangle3d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Spline2d exposing (Spline2d)
import Spline3d exposing (Spline3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)


{-| Fuzz a list of 0 to 32 elements.
-}
smallList : Fuzzer a -> Fuzzer (List a)
smallList itemFuzzer =
    Fuzz.listOfLengthBetween 0 32 itemFuzzer


{-| Fuzz a parameter value between 0 and 1.
-}
parameterValue : Fuzzer Float
parameterValue =
    Fuzz.frequency
        [ ( 1, Fuzz.floatRange 0 1 )
        , ( 1, Fuzz.constant 0 )
        , ( 1, Fuzz.constant 1 )
        ]


{-| Fuzz a value between -10 and +10 to use for scaling geometry. (Note that this includes 0.)
-}
scale : Fuzzer Float
scale =
    Fuzz.frequency
        [ ( 3, Fuzz.floatRange -10 10 )
        , ( 1, Fuzz.constant 0 )
        ]


{-| Fuzz a length between -10 meters and +10 meters.
-}
length : Fuzzer Length
length =
    Fuzz.map Length.meters (Fuzz.floatRange -10 10)

{-| Fuzz a length between -10 meters and +10 meters (excluding 0).
-}
nonzeroLength : Fuzzer Length
nonzeroLength =
    Fuzz.floatRange -10 10
        |> Fuzz.filter (\n -> n /= 0)
        |> Fuzz.map Length.meters


{-| Fuzz a random unitless quantity between -10 and +10.
-}
unitlessQuantity : Fuzzer (Quantity Float Unitless)
unitlessQuantity =
    Fuzz.map Quantity.float scale


{-| Fuzz a random unitless interval with endpoints between -10 and +10.
-}
unitlessInterval : Fuzzer (Interval Float Unitless)
unitlessInterval =
    Fuzz.map2 Interval.from unitlessQuantity unitlessQuantity


{-| Fuzz a random length between 0 and 10 meters.
-}
positiveLength : Fuzzer Length
positiveLength =
    Fuzz.map Quantity.abs length


{-| Fuzz a random length between 0 and 10 meters (excluding 0).
-}
nonzeroPositiveLength : Fuzzer Length
nonzeroPositiveLength =
    Fuzz.floatRange -10 10
        |> Fuzz.filter (\n -> n > 0)
        |> Fuzz.map Length.meters


{-| Fuzz a random angle between -360 and +360 degrees.
-}
angle : Fuzzer Angle
angle =
    Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))


{-| Fuzz a random 2D direction.
-}
direction2d : Fuzzer (Direction2d coordinates)
direction2d =
    Fuzz.map Direction2d.fromAngle angle


{-| Fuzz a random 3D direction.
-}
direction3d : Fuzzer (Direction3d coordinates)
direction3d =
    let
        phiFuzzer =
            Fuzz.map (acos >> Angle.radians) (Fuzz.floatRange -1 1)

        thetaFuzzer =
            Fuzz.map Angle.radians (Fuzz.floatRange -pi pi)

        toDirection phi theta =
            let
                r =
                    Angle.sin phi
            in
            Direction3d.unsafe
                { x = r * Angle.cos theta
                , y = r * Angle.sin theta
                , z = Angle.cos phi
                }
    in
    Fuzz.map2 toDirection phiFuzzer thetaFuzzer


{-| Fuzz a random 2D vector with components in the range -10 to +10 meters.
-}
vector2d : Fuzzer (Vector2d Meters coordinates)
vector2d =
    Fuzz.map2 Vector2d.xy length length


{-| Fuzz a random 3D vector with components in the range -10 to +10 meters.
-}
vector3d : Fuzzer (Vector3d Meters coordinates)
vector3d =
    Fuzz.map3 Vector3d.xyz length length length


{-| -}
boundingBox2d : Fuzzer (BoundingBox2d Meters coordinates)
boundingBox2d =
    Fuzz.map2 BoundingBox2d.from point2d point2d


{-| -}
boundingBox3d : Fuzzer (BoundingBox3d Meters coordinates)
boundingBox3d =
    Fuzz.map2 BoundingBox3d.from point3d point3d


{-| -}
vectorBoundingBox2d : Fuzzer (VectorBoundingBox2d Meters coordinates)
vectorBoundingBox2d =
    Fuzz.map2 VectorBoundingBox2d.hull2 vector2d vector2d


{-| -}
vectorBoundingBox3d : Fuzzer (VectorBoundingBox3d Meters coordinates)
vectorBoundingBox3d =
    Fuzz.map2 VectorBoundingBox3d.hull2 vector3d vector3d


{-| Fuzz a random 2D point with components in the range -10 to +10 meters.
-}
point2d : Fuzzer (Point2d Meters coordinates)
point2d =
    Fuzz.map2 Point2d.xy length length


{-| Fuzz a random 3D point with components in the range -10 to +10 meters.
-}
point3d : Fuzzer (Point3d Meters coordinates)
point3d =
    Fuzz.map3 Point3d.xyz length length length


{-| -}
axis2d : Fuzzer (Axis2d Meters coordinates)
axis2d =
    Fuzz.map2 Axis2d.through point2d direction2d


{-| -}
axis3d : Fuzzer (Axis3d Meters coordinates)
axis3d =
    Fuzz.map2 Axis3d.through point3d direction3d


{-| -}
frame2d : Fuzzer (Frame2d Meters coordinates { defines : localCoordinates })
frame2d =
    let
        frame originPoint xDirection rightHanded =
            let
                rightHandedFrame =
                    Frame2d.withXDirection xDirection originPoint
            in
            if rightHanded then
                rightHandedFrame

            else
                Frame2d.reverseY rightHandedFrame
    in
    Fuzz.map3 frame point2d direction2d Fuzz.bool


{-| -}
frame3d : Fuzzer (Frame3d Meters coordinates { defines : localCoordinates })
frame3d =
    let
        frame originPoint xDirection reverseY reverseZ =
            let
                ( yDirection, zDirection ) =
                    Direction3d.perpendicularBasis xDirection
            in
            Frame3d.unsafe
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection =
                    if reverseY then
                        Direction3d.reverse yDirection

                    else
                        yDirection
                , zDirection =
                    if reverseZ then
                        Direction3d.reverse zDirection

                    else
                        zDirection
                }
    in
    Fuzz.map4 frame point3d direction3d Fuzz.bool Fuzz.bool


{-| -}
lineSegment2d : Fuzzer (LineSegment2d Meters coordinates)
lineSegment2d =
    Fuzz.map2 LineSegment2d.from point2d point2d


{-| -}
lineSegment3d : Fuzzer (LineSegment3d Meters coordinates)
lineSegment3d =
    Fuzz.map2 LineSegment3d.from point3d point3d


{-| -}
triangle2d : Fuzzer (Triangle2d Meters coordinates)
triangle2d =
    Fuzz.map3 Triangle2d.from point2d point2d point2d


{-| -}
triangle3d : Fuzzer (Triangle3d Meters coordinates)
triangle3d =
    Fuzz.map3 Triangle3d.from point3d point3d point3d


{-| -}
rectangle2d : Fuzzer (Rectangle2d Meters coordinates)
rectangle2d =
    let
        rectangle axes width height =
            Rectangle2d.centeredOn axes ( width, height )
    in
    Fuzz.map3 rectangle frame2d positiveLength positiveLength


{-| -}
nondegenerateRectangle2d : Fuzzer (Rectangle2d Meters coordinates)
nondegenerateRectangle2d =
    let
        rectangle axes width height =
            Rectangle2d.centeredOn axes ( width, height )
    in
    Fuzz.map3 rectangle frame2d nonzeroPositiveLength nonzeroPositiveLength


{-| -}
rectangle3d : Fuzzer (Rectangle3d Meters coordinates)
rectangle3d =
    Fuzz.map2 Rectangle3d.on sketchPlane3d rectangle2d


{-| -}
quadraticSpline2d : Fuzzer (QuadraticSpline2d Meters coordinates)
quadraticSpline2d =
    Fuzz.map3 QuadraticSpline2d.fromControlPoints point2d point2d point2d


{-| -}
quadraticSpline3d : Fuzzer (QuadraticSpline3d Meters coordinates)
quadraticSpline3d =
    Fuzz.map3 QuadraticSpline3d.fromControlPoints point3d point3d point3d


{-| -}
cubicSpline2d : Fuzzer (CubicSpline2d Meters coordinates)
cubicSpline2d =
    Fuzz.map4 CubicSpline2d.fromControlPoints point2d point2d point2d point2d


weightedControlPoint2d : Fuzzer ( Point2d Meters coordinates, Float )
weightedControlPoint2d =
    Fuzz.pair point2d (Fuzz.floatRange 1 10)


{-| -}
rationalQuadraticSpline2d : Fuzzer (RationalQuadraticSpline2d Meters coordinates)
rationalQuadraticSpline2d =
    Fuzz.map3 RationalQuadraticSpline2d.fromControlPoints
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d


{-| -}
rationalCubicSpline2d : Fuzzer (RationalCubicSpline2d Meters coordinates)
rationalCubicSpline2d =
    Fuzz.map4 RationalCubicSpline2d.fromControlPoints
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d


weightedControlPoint3d : Fuzzer ( Point3d Meters coordinates, Float )
weightedControlPoint3d =
    Fuzz.pair point3d (Fuzz.floatRange 1 10)


{-| -}
rationalQuadraticSpline3d : Fuzzer (RationalQuadraticSpline3d Meters coordinates)
rationalQuadraticSpline3d =
    Fuzz.map3 RationalQuadraticSpline3d.fromControlPoints
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d


{-| -}
rationalCubicSpline3d : Fuzzer (RationalCubicSpline3d Meters coordinates)
rationalCubicSpline3d =
    Fuzz.map4 RationalCubicSpline3d.fromControlPoints
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d


{-| -}
cubicSpline3d : Fuzzer (CubicSpline3d Meters coordinates)
cubicSpline3d =
    Fuzz.map4 CubicSpline3d.fromControlPoints point3d point3d point3d point3d


{-| -}
spline3d : Fuzzer (Spline3d Meters coordinates)
spline3d =
    Fuzz.map2 Spline3d.fromControlPoints
        point3d
        (Fuzz.listOfLengthBetween 0 12 point3d)


{-| -}
spline2d : Fuzzer (Spline2d Meters coordinates)
spline2d =
    Fuzz.map2 Spline2d.fromControlPoints
        point2d
        (Fuzz.listOfLengthBetween 0 12 point2d)


{-| -}
plane3d : Fuzzer (Plane3d Meters coordinates)
plane3d =
    Fuzz.map2 Plane3d.through point3d direction3d


{-| -}
sketchPlane3d : Fuzzer (SketchPlane3d Meters coordinates { defines : sketchCoordinates })
sketchPlane3d =
    let
        sketchPlane plane rotationAngle =
            SketchPlane3d.fromPlane plane
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.normalAxis rotationAngle
    in
    Fuzz.map2 sketchPlane plane3d angle


{-| -}
arc2d : Fuzzer (Arc2d Meters coordinates)
arc2d =
    Fuzz.map3 Arc2d.from
        point2d
        point2d
        (Fuzz.oneOf
            [ Fuzz.floatRange -350 350
            , Fuzz.floatRange 370 710
            , Fuzz.floatRange -710 -370
            ]
            |> Fuzz.map Angle.degrees
        )


{-| -}
arc3d : Fuzzer (Arc3d Meters coordinates)
arc3d =
    Fuzz.map2 Arc3d.on sketchPlane3d arc2d


{-| -}
ellipse2d : Fuzzer (Ellipse2d Meters coordinates)
ellipse2d =
    let
        ellipse centerPoint xDirection xRadius yRadius =
            Ellipse2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                }
    in
    Fuzz.map4 ellipse point2d direction2d positiveLength positiveLength


{-| -}
ellipticalArc2d : Fuzzer (EllipticalArc2d Meters coordinates)
ellipticalArc2d =
    let
        ellipticalArc ( centerPoint, xDirection ) ( xRadius, yRadius ) ( startAngle, sweptAngle ) =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                , startAngle = startAngle
                , sweptAngle = sweptAngle
                }
    in
    Fuzz.map3 ellipticalArc
        (Fuzz.pair point2d direction2d)
        (Fuzz.pair positiveLength positiveLength)
        (Fuzz.pair angle angle)


{-| -}
ellipticalArc3d : Fuzzer (EllipticalArc3d Meters coordinates)
ellipticalArc3d =
    Fuzz.map2 EllipticalArc3d.on sketchPlane3d ellipticalArc2d


{-| -}
sphere3d : Fuzzer (Sphere3d Meters coordinates)
sphere3d =
    Fuzz.map2 Sphere3d.withRadius positiveLength point3d


{-| -}
block3d : Fuzzer (Block3d Meters coordinates)
block3d =
    let
        block axes xDim yDim zDim =
            Block3d.centeredOn axes ( xDim, yDim, zDim )
    in
    Fuzz.map4 block frame3d positiveLength positiveLength positiveLength


{-| -}
circle2d : Fuzzer (Circle2d Meters coordinates)
circle2d =
    Fuzz.map2 Circle2d.withRadius positiveLength point2d


{-| -}
circle3d : Fuzzer (Circle3d Meters coordinates)
circle3d =
    Fuzz.map3 Circle3d.withRadius positiveLength direction3d point3d


{-| -}
cone3d : Fuzzer (Cone3d Meters coordinates)
cone3d =
    let
        cone basePoint direction coneLength coneRadius =
            Cone3d.startingAt basePoint direction <|
                { length = coneLength
                , radius = coneRadius
                }
    in
    Fuzz.map4 cone point3d direction3d positiveLength positiveLength


{-| -}
cylinder3d : Fuzzer (Cylinder3d Meters coordinates)
cylinder3d =
    let
        cylinder centerPoint direction cylinderLength cylinderRadius =
            Cylinder3d.centeredOn centerPoint direction <|
                { length = cylinderLength
                , radius = cylinderRadius
                }
    in
    Fuzz.map4 cylinder point3d direction3d positiveLength positiveLength


{-| -}
ellipsoid3d : Fuzzer (Ellipsoid3d Meters coordinates)
ellipsoid3d =
    let
        ellipsoid axes xRadius yRadius zRadius =
            Ellipsoid3d.with
                { axes = axes
                , xRadius = xRadius
                , yRadius = yRadius
                , zRadius = zRadius
                }
    in
    Fuzz.map4 ellipsoid
        frame3d
        positiveLength
        positiveLength
        positiveLength


{-| The `polygon2d` fuzzer is a bit special. Since randomly generating vertices and then
connecting them would almost always result in useless self-intersecting polygons, this instead
generates polygons of a few different types that are non-self-intersecting by construction:

  - A donut-shaped polygon with a hole in the middle
  - An L-shaped polygon
  - A roughly square-shaped polygon
  - A few variations on square-ish polygons with one or two holes

All the polygons will be roughly centered on the origin.

-}
polygon2d : Fuzzer (Polygon2d Meters coordinates)
polygon2d =
    Polygon2d.Random.polygon2d <|
        BoundingBox2d.fromExtrema
            { minX = Length.meters -10
            , maxX = Length.meters 10
            , minY = Length.meters -10
            , maxY = Length.meters 10
            }
