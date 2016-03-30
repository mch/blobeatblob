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

type alias Model =
  { windowSize : (Int, Int)
  , blobPosition : (Float, Float)
  , food : List Food.Food
  }


type alias Inputs =
  { x : Int
  , y : Int
  , dt : Float
  }


type Action
  = Input Inputs


init : (Model, Effects.Effects Action)
init =
  ({ windowSize = (800, 600)
   , blobPosition = (0, 0)
   , food = Food.init 800 600
   }
  , Effects.none
  )


noFx : Model -> (Model, Effects.Effects Action)
noFx model =
  ( model, Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Input inputs -> noFx (updateBlobPositionWithKeys model inputs)


updateBlobPositionWithKeys model keys =
  let
    (bx, by) = model.blobPosition
  in
    { model | blobPosition = (bx + 10 * (toFloat keys.x), by + 10 * (toFloat keys.y)) }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (wx, wy) = model.windowSize
    blob = filled Color.red (circle 50)
    translatedBlob = move model.blobPosition blob
    food = List.map drawFood model.food
  in
    Html.fromElement (collage wx wy (translatedBlob :: food))


drawFood food =
  circle 10
    |> filled food.colour
    |> move food.position


timer = Time.fps 30


inputs =
  [ Signal.map Input (Signal.map3 Inputs
                        (Signal.map .x Keyboard.wasd)
                        (Signal.map .y Keyboard.wasd)
                        timer)
  ]


app =
    start { init = init
          , update = update
          , view = view
          , inputs = inputs
          }


main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
