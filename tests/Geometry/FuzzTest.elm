module Geometry.FuzzTest exposing
    ( check, check2, check3, check4, check5, check6, check7, check8
    )

{-| Same ergonomics as `Test.Random.check*` helpers, but for `Fuzzer` inputs and `Test.fuzz*`
rather than `Random.Generator` inputs and `Fuzz.fromGenerator`.
-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


check : String -> Fuzzer a -> (a -> Expectation) -> Test
check description fuzzer expectation =
    Test.fuzz fuzzer description expectation


check2 : String -> Fuzzer a -> Fuzzer b -> (a -> b -> Expectation) -> Test
check2 description fuzzA fuzzB expectation =
    Test.fuzz2 fuzzA fuzzB description expectation


check3 : String -> Fuzzer a -> Fuzzer b -> Fuzzer c -> (a -> b -> c -> Expectation) -> Test
check3 description fuzzA fuzzB fuzzC expectation =
    Test.fuzz3 fuzzA fuzzB fuzzC description expectation


check4 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> (a -> b -> c -> d -> Expectation)
    -> Test
check4 description fuzzA fuzzB fuzzC fuzzD expectation =
    Test.fuzz
        (Fuzz.map4
            (\a b c d ->
                { a = a, b = b, c = c, d = d }
            )
            fuzzA
            fuzzB
            fuzzC
            fuzzD
        )
        description
        (\r -> expectation r.a r.b r.c r.d)


check5 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> (a -> b -> c -> d -> e -> Expectation)
    -> Test
check5 description fuzzA fuzzB fuzzC fuzzD fuzzE expectation =
    Test.fuzz
        (Fuzz.map5
            (\a b c d e ->
                { a = a, b = b, c = c, d = d, e = e }
            )
            fuzzA
            fuzzB
            fuzzC
            fuzzD
            fuzzE
        )
        description
        (\r -> expectation r.a r.b r.c r.d r.e)


check6 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> Fuzzer f
    -> (a -> b -> c -> d -> e -> f -> Expectation)
    -> Test
check6 description fuzzA fuzzB fuzzC fuzzD fuzzE fuzzF expectation =
    Test.fuzz
        (Fuzz.map6
            (\a b c d e f ->
                { a = a, b = b, c = c, d = d, e = e, f = f }
            )
            fuzzA
            fuzzB
            fuzzC
            fuzzD
            fuzzE
            fuzzF
        )
        description
        (\r -> expectation r.a r.b r.c r.d r.e r.f)


check7 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> Fuzzer f
    -> Fuzzer g
    -> (a -> b -> c -> d -> e -> f -> g -> Expectation)
    -> Test
check7 description fuzzA fuzzB fuzzC fuzzD fuzzE fuzzF fuzzG expectation =
    Test.fuzz
        (Fuzz.map7
            (\a b c d e f g ->
                { a = a, b = b, c = c, d = d, e = e, f = f, g = g }
            )
            fuzzA
            fuzzB
            fuzzC
            fuzzD
            fuzzE
            fuzzF
            fuzzG
        )
        description
        (\r -> expectation r.a r.b r.c r.d r.e r.f r.g)


check8 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> Fuzzer f
    -> Fuzzer g
    -> Fuzzer h
    -> (a -> b -> c -> d -> e -> f -> g -> h -> Expectation)
    -> Test
check8 description fuzzA fuzzB fuzzC fuzzD fuzzE fuzzF fuzzG fuzzH expectation =
    Test.fuzz
        (Fuzz.map8
            (\a b c d e f g h ->
                { a = a, b = b, c = c, d = d, e = e, f = f, g = g, h = h }
            )
            fuzzA
            fuzzB
            fuzzC
            fuzzD
            fuzzE
            fuzzF
            fuzzG
            fuzzH
        )
        description
        (\r -> expectation r.a r.b r.c r.d r.e r.f r.g r.h)
