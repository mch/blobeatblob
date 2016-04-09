module Tests where

import ElmTest exposing (..)
import Collidable exposing (..)
import String


baz1 = { position = (0,0), radius = 5, name = "1" }
baz2 = { position = (11,0), radius = 5, name = "2" }
baz3 = { position = (10,0), radius = 5, name = "3" }
baz4 = { position = (9,0), radius = 5, id = 4 }


type alias Bar =
  { position : ( Float, Float )
  , radius : Float
  , name : String
  , id : Int
  }


bar1 = Bar (0,0) 4 "one" 1
bar2 = Bar (0,8) 4 "two" 2
bar3 = Bar (0,7.9) 4 "three" 3
bar4 = Bar (0,9) 4 "four" 4


circularRegionTests : Test
circularRegionTests =
    suite "Circular bounding region tests"
        [ test "x offset circles no collision"
            (assert (not (circlesDidCollide baz1 baz2)))
        , test "x offset circles collided"
            (assert (circlesDidCollide baz1 baz3))
        , test "x offset circles of different types collided"
            (assert (circlesDidCollide baz1 baz4))
        , test "y offset touching circles collided"
            (assert (circlesDidCollide bar1 bar2))
        , test "y offset overlapping circles collided"
            (assert (circlesDidCollide bar1 bar3))
        , test "y offset circles did not collide"
            (assert (not (circlesDidCollide bar1 bar4)))
        ]


type alias Foo =
  { position : ( Float, Float )
  , width : Float
  , height : Float
  , name : String
  }


foo1 = Foo (0,0) 10 10 "one"
foo2 = Foo (11,0) 10 10 "two"
foo3 = Foo (10,0) 10 10 "three"
foo4 = Foo (-11,0) 10 10 "four"
foo5 = Foo (-10,0) 10 10 "five"
foo6 = Foo (0,11) 10 10 "six"
foo7 = Foo (0,10) 10 10 "seven"
foo8 = Foo (0,-11) 10 10 "eight"
foo9 = Foo (0,-10) 10 10 "nine"


rectangularRegionTests : Test
rectangularRegionTests =
  suite "Rectangular bounding region tests"
      [ test "positive x offset not touching"
          (assert (not (boxesDidCollide foo1 foo2)))
      , test "positive x offset touching"
          (assert (boxesDidCollide foo1 foo3))
      , test "negative x offset not touching"
          (assert (not (boxesDidCollide foo1 foo4)))
      , test "negative x offset touching"
          (assert (boxesDidCollide foo1 foo5))
      , test "positive y offset not touching"
          (assert (not (boxesDidCollide foo1 foo6)))
      , test "positive y offset touching"
          (assert (boxesDidCollide foo1 foo7))
      , test "negative y offset not touching"
          (assert (not (boxesDidCollide foo1 foo8)))
      , test "negative y offset touching"
          (assert (boxesDidCollide foo1 foo9))
      ]

all : Test
all = suite "All tests"
          [ circularRegionTests
          , rectangularRegionTests
          ]
