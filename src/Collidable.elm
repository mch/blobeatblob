module Collidable (..) where


type alias BoundingCircle a =
  { a
    | position : (Float, Float)
    , radius : Float }


type alias BoundingBox a =
  { a
    | position : (Float, Float)
    , width : Float
    , height : Float }


circlesDidCollide : BoundingCircle a -> BoundingCircle b -> Bool
circlesDidCollide c1 c2 =
  let
    distanceSquared : ( Float, Float ) -> ( Float, Float ) -> Float
    distanceSquared ( x1, y1 ) ( x2, y2 ) =
      (x1 - x2) ^ 2 + (y1 - y2) ^ 2

    collisionDistanceSquared = (c1.radius + c2.radius) ^ 2
  in
    (distanceSquared c1.position c2.position) <= collisionDistanceSquared


boxesDidCollide : BoundingBox a -> BoundingBox b -> Bool
boxesDidCollide b1 b2 =
  let
    (bx1, by1) =
      b1.position

    (bx2, by2) =
      b2.position

    touchingAlongX =
      if bx1 < bx2 then
        (bx1 + b1.width / 2) >= (bx2 - b2.width / 2)
      else
        (bx2 + b2.width / 2) >= (bx1 - b1.width / 2)

    touchingAlongY =
      if by1 < by2 then
        (by1 + b1.height / 2) >= (by2 - b2.height / 2)
      else
        (by2 + b2.height / 2) >= (by1 - b1.height / 2)

  in
    touchingAlongX && touchingAlongY
