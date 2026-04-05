module Tests.DelaunayTriangulation2d exposing
    ( allDelaunayTrianglesHaveNonzeroArea
    , delaunayTriangleContainsOnlyItsVertices
    , failsOnCoincidentVertices
    )

import Array exposing (Array)
import Circle2d
import DelaunayTriangulation2d
import Expect
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Geometry.FuzzTest as Test
import Length exposing (Meters, inMeters)
import List.Extra
import Point2d exposing (Point2d)
import Quantity
import Test exposing (Test)
import Triangle2d


uniquePoints : Fuzzer (Array (Point2d Meters coordinates))
uniquePoints =
    Random.smallList Random.point2d
        |> Fuzz.map (List.Extra.uniqueBy (Point2d.toTuple inMeters))
        |> Fuzz.map Array.fromList


allDelaunayTrianglesHaveNonzeroArea : Test
allDelaunayTrianglesHaveNonzeroArea =
    let
        description =
            "The delaunay triangulation only produces triangles with non-zero area"

        expectation points =
            case DelaunayTriangulation2d.fromPoints points of
                Err _ ->
                    Expect.pass

                Ok triangulation ->
                    let
                        triangles =
                            DelaunayTriangulation2d.triangles triangulation

                        hasNonPositiveArea triangle =
                            Triangle2d.area triangle
                                |> Quantity.lessThanOrEqualTo Quantity.zero
                    in
                    case List.filter hasNonPositiveArea triangles of
                        [] ->
                            Expect.pass

                        x :: xs ->
                            Expect.fail ("DelaunayTriangulation2d produced a triangle with negative or zero area: " ++ Debug.toString x)
    in
    Test.check description uniquePoints expectation


delaunayTriangleContainsOnlyItsVertices : Test
delaunayTriangleContainsOnlyItsVertices =
    let
        description =
            "A delaunay triangle's circumcircle only contains its three vertices, no other points"

        expectation points =
            let
                check triangle =
                    case Triangle2d.circumcircle triangle of
                        Nothing ->
                            Err ("A delaunay triangle is degenerate: " ++ Debug.toString triangle)

                        Just circle ->
                            let
                                ( p1, p2, p3 ) =
                                    Triangle2d.vertices triangle

                                predicate point =
                                    Circle2d.contains point circle
                                        && (point /= p1)
                                        && (point /= p2)
                                        && (point /= p3)

                                containedPoints =
                                    Array.filter predicate points
                            in
                            if Array.isEmpty containedPoints then
                                Ok ()

                            else
                                Err ("A delaunay triangle circumcircle contains non-vertex points " ++ Debug.toString containedPoints)

                checkAll remainingTriangles =
                    case remainingTriangles of
                        [] ->
                            Expect.pass

                        triangle :: rest ->
                            case check triangle of
                                Ok _ ->
                                    checkAll rest

                                Err errorMessage ->
                                    Expect.fail errorMessage
            in
            case DelaunayTriangulation2d.fromPoints points of
                Err _ ->
                    Expect.pass

                Ok triangulation ->
                    checkAll (DelaunayTriangulation2d.triangles triangulation)
    in
    Test.check description uniquePoints expectation


failsOnCoincidentVertices : Test
failsOnCoincidentVertices =
    let
        description =
            "Delaunay triangulation construction should fail when coincident vertices are given"

        expectation points =
            case points of
                [] ->
                    Expect.pass

                x :: xs ->
                    let
                        pointsWithDuplicate =
                            Array.fromList (x :: x :: xs)
                    in
                    DelaunayTriangulation2d.fromPoints pointsWithDuplicate
                        |> Expect.err
    in
    Test.check description (Random.smallList Random.point2d) expectation
