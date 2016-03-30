module Food (Food, init, view, update) where

import Graphics.Collage exposing (..)
import Random exposing (..)
import Color
import Time


type alias Food =
  { seed : Seed
  , particles : List FoodParticle
  , maxNumParticles : Int
  , width : Float
  , height : Float
  , addFoodState : Maybe Time.Time
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


init : Float -> Float -> Int -> Food
init width height numParticles =
  let
    -- TODO: take seed from a port to get current time.
    seed = initialSeed 1
    food = Food seed [] numParticles width height Nothing
  in
    generateParticles food


generateParticles : Food -> Food
generateParticles food =
  let
    foodSize = 10
    numParticles = food.maxNumParticles - (List.length food.particles)

    xgen = float (-1 * food.width/2) (food.width/2)
    ygen = float (-1 * food.height/2) (food.height/2)
    pointgen = pair xgen ygen
    (pointList, seed') = generate (list numParticles pointgen) food.seed
    (colourIndexList, seed'') = generate (list numParticles <| int 1 4) seed'
    outlineColourList = List.map int2darkcolour colourIndexList
    fillColourList = List.map int2lightcolour colourIndexList
    particles = List.map4 FoodParticle fillColourList outlineColourList pointList (List.repeat numParticles foodSize)
  in
    { food
      | particles = particles ++ food.particles
      , seed = seed''
    }


update : Food -> Time.Time -> Food
update food dt =
  case food.addFoodState of
    Nothing ->
      if List.length food.particles < 20 then
        { food | addFoodState = Just 0 }
      else
        food

    Just t ->
      if t > Time.second then
        let
          newFood = generateParticles food
        in
          { newFood | addFoodState = Nothing }
      else
        { food | addFoodState = Just (t + dt) }


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
