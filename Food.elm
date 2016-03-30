module Food (Food, init, view) where

import Graphics.Collage exposing (..)
import Random exposing (int, float, pair, list, map, generate, initialSeed)
import Color


type alias Food =
  { fillColour : Color.Color
  , outlineColour : Color.Color
  , position : (Float, Float)
  , size : Float
  }


int2lightcolour i =
  case i of
    1 -> Color.lightRed
    2 -> Color.lightOrange
    3 -> Color.lightYellow
    4 -> Color.lightGreen
    _ -> Color.lightBlue


int2darkcolour i =
  case i of
    1 -> Color.darkRed
    2 -> Color.darkOrange
    3 -> Color.darkYellow
    4 -> Color.darkGreen
    _ -> Color.darkBlue


init : Float -> Float -> List Food
init width height =
  let
    xgen = float (-1 * width/2) (width/2)
    ygen = float (-1 * height/2) (height/2)
    pointgen = pair xgen ygen
    seed = initialSeed 1
    (pointList, seed') = generate (list 20 pointgen) seed
    (colourIndexList, seed'') = generate (list 20 <| int 1 4) seed'
    outlineColourList = List.map int2darkcolour colourIndexList
    fillColourList = List.map int2lightcolour colourIndexList
  in
    List.map4 Food fillColourList outlineColourList pointList (List.repeat 20 10)


view : List Food -> List Form
view food =
  List.map drawFood food


drawFood : Food -> Form
drawFood food =
  let
    outer = circle food.size
          |> filled food.outlineColour
          |> move food.position
    inner = circle (food.size - 2)
            |> filled food.fillColour
            |> move food.position
  in
    group [outer, inner]
