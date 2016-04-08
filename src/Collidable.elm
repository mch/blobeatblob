module Collidable (..) where

type alias Collidable = { position : (Float, Float), size : Float }

-- Returns a list of pairs of objects that have collided
detectCollisionsForCircles : List Collidable -> List ( Collidable, Collidable )
detectCollisionsForCircles circles =
  let
    distance : ( Float, Float ) -> ( Float, Float ) -> Float
    distance ( x1, y1 ) ( x2, y2 ) =
      sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

    didCollide : Collidable -> Collidable -> List (Collidable, Collidable)
    didCollide c1 c2 =
      if distance c1.position c2.position <= c1.size + c2.size then
        [(c1, c2)]
      else
        []

    checkPairs : List Collidable -> List (Collidable, Collidable)
    checkPairs l =
      case l of
        h :: t -> List.append (List.concatMap (didCollide h) t) (checkPairs t)
        [] -> []
  in
    Debug.log "collisions" (checkPairs circles)

