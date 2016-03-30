module Food (Food, init, view) where

import Graphics.Collage exposing (..)
import Random exposing (..)
import Color


type alias Food =
  { seed : Seed
  , particles : List FoodParticle
  }


type alias FoodParticle =
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


init : Float -> Float -> Food
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
    particles = List.map4 FoodParticle fillColourList outlineColourList pointList (List.repeat 20 10)
  in
    Food seed'' particles


view : Food -> List Form
view food =
  List.map drawParticle food.particles


drawParticle : FoodParticle -> Form
drawParticle particle =
  let
    outer = circle particle.size
          |> filled particle.outlineColour
          |> move particle.position
    inner = circle (particle.size - 2)
            |> filled particle.fillColour
            |> move particle.position
  in
    group [outer, inner]
