module Tests where

import ElmTest exposing (..)
import Collidable exposing (..)
import String

type alias Thing = { name : String }
type alias SimpleThing = { position : (Float, Float), size : Float }

makeThing : (Float, Float) -> Float -> String -> Collidable Thing
makeThing p s n =
  { position = p, size = s, name = n }

thing1 : Collidable Thing
thing1 = makeThing (0, 0) 5 "Thing One"
thing2 : Collidable Thing
thing2 = makeThing (5, 0) 5 "Thing Two"

collidable1 : Collidable {}
collidable1 = { position = (1, 2), size = 5 }

makeCollidable : (Float, Float) -> Float -> Collidable {}
makeCollidable p r =
  { position = p, size = r }

all : Test
all =
    suite "A Test Suite"
        [ test "Empty list"
            (assertEqual [] (detectCollisionsForCircles []))
        , test "No collisions with self"
            (assertEqual [] (detectCollisionsForCircles [collidable1]))
        , test "No collision"
            (assertEqual [] (detectCollisionsForCircles [makeCollidable (0, 0) 5, makeCollidable (0, 11) 5]))
        , test "Close collision"
            (assertEqual [(makeCollidable (0, 0) 5, makeCollidable (0, 10) 5)]
               (detectCollisionsForCircles [makeCollidable (0, 0) 5, makeCollidable (0, 10) 5]))
        , test "Overlapping Collision"
            (assertEqual [(makeCollidable (0, 0) 5, makeCollidable (0, 9) 5)]
               (detectCollisionsForCircles [makeCollidable (0, 0) 5, makeCollidable (0, 9) 5]))
        , test "Extended records"
            (assertEqual [(thing1, thing2)]
               (detectCollisionsForCircles [thing1, thing2]))
        ]
