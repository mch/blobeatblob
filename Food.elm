module Food (Food, init) where

import Random exposing (int, float, pair, list, map, generate, initialSeed)
import Color


type alias Food =
  { colour : Color.Color
  , position : (Float, Float)
  }


randomColours i =
  case i of
    1 -> Color.red
    2 -> Color.orange
    3 -> Color.yellow
    4 -> Color.green
    _ -> Color.blue


init : Float -> Float -> List Food
init width height =
  let
    xgen = float (-1 * width/2) (width/2)
    ygen = float (-1 * height/2) (height/2)
    pointgen = pair xgen ygen
    seed = initialSeed 1
    (pointlist, seed') = generate (list 20 pointgen) seed
    (colourindexlist, seed'') = generate (list 20 <| int 1 4) seed'
    colourlist = List.map randomColours colourindexlist
  in
    List.map2 Food colourlist pointlist
