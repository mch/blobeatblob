module Main (..) where

import Graphics.Collage exposing (..)
import Color
import Time
import Keyboard
import Effects
import Html
import Signal
import Task
import StartApp exposing (start)
import Food
import Blob


type alias Model =
  { windowSize : ( Int, Int )
  , blob : Blob.Blob
  , food : Food.Food
  , score : Int
  }


type alias Inputs =
  { x : Int
  , y : Int
  , dt : Float
  }


type Action
  = Input Inputs


init : ( Model, Effects.Effects Action )
init =
  let
    windowWidth =
      800

    windowHeight =
      600

    startingSize =
      11

    numFoodParticles =
      20
  in
    ( { windowSize = ( windowWidth, windowHeight )
      , blob = Blob.Blob ( 0, 0 ) startingSize
      , food = Food.init windowWidth windowHeight numFoodParticles
      , score = 0
      }
    , Effects.none
    )


noFx : Model -> ( Model, Effects.Effects Action )
noFx model =
  ( model, Effects.none )


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Input inputs ->
      noFx (updateModel model inputs)


updateModel : Model -> Inputs -> Model
updateModel model inputs =
  let
    newModel =
      updateBlobPositionWithKeys model inputs
        |> detectCollisions

    newFood =
      Food.update newModel.food inputs.dt
  in
    { newModel | food = newFood }


updateBlobPositionWithKeys : Model -> Inputs -> Model
updateBlobPositionWithKeys model keys =
  let
    blob =
      model.blob

    ( bx, by ) =
      blob.position

    newX =
      bx + 10 * (toFloat keys.x)

    newY =
      by + 10 * (toFloat keys.y)

    newBlob =
      { blob | position = ( newX, newY ) }
  in
    { model | blob = newBlob }


detectCollisions : Model -> Model
detectCollisions model =
  let
    ( bx, by ) =
      model.blob.position

    distance ( fx, fy ) =
      sqrt ((bx - fx) ^ 2 + (by - fy) ^ 2)

    didCollide f =
      (distance f.position) > (f.size + model.blob.size)

    newParticles =
      List.filter didCollide model.food.particles

    food =
      model.food

    newFood =
      { food | particles = newParticles }

    points =
      (List.length model.food.particles) - (List.length newParticles)

    blob =
      model.blob

    newBlob =
      { blob | size = blob.size + (toFloat points) }
  in
    { model
      | food = newFood
      , score = model.score + points
      , blob = newBlob
    }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    ( wx, wy ) =
      model.windowSize

    blob =
      Blob.view model.blob

    food =
      Food.view model.food
  in
    Html.div
      []
      [ Html.fromElement (collage wx wy (blob :: food))
      , Html.p [] [ Html.text ("Score: " ++ (toString model.score)) ]
      ]


timer : Signal Time.Time
timer =
  Time.fps 30


inputs : List (Signal Action)
inputs =
  [ Signal.map
      Input
      (Signal.map3
        Inputs
        (Signal.map .x Keyboard.wasd)
        (Signal.map .y Keyboard.wasd)
        timer
      )
  ]


app : StartApp.App Model
app =
  start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
