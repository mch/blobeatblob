module Tests where

import ElmTest exposing (..)
import Collidable exposing (..)
import String


all : Test
all =
    suite "A Test Suite"
        [ test "Empty list"
            (assertEqual [] (detectCollisionsForCircles []))
        , test "No collisions with self"
            (assertEqual [] (detectCollisionsForCircles [Collidable (1, 2) 5]))
        , test "No collision"
            (assertEqual [] (detectCollisionsForCircles [Collidable (0, 0) 5, Collidable (0, 11) 5]))
        , test "Close collision"
            (assertEqual [(Collidable (0, 0) 5, Collidable (0, 10) 5)]
               (detectCollisionsForCircles [Collidable (0, 0) 5, Collidable (0, 10) 5]))
        , test "Overlapping Collision"
            (assertEqual [(Collidable (0, 0) 5, Collidable (0, 9) 5)]
               (detectCollisionsForCircles [Collidable (0, 0) 5, Collidable (0, 9) 5]))
        ]
