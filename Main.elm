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


type alias Blob =
  { position : (Float, Float)
  , size : Float
  }


type alias Model =
  { windowSize : (Int, Int)
  , blob : Blob
  , food : List Food.Food
  , score : Int
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
   , blob = Blob (0, 0) 50
   , food = Food.init 800 600
   , score = 0
   }
  , Effects.none
  )


noFx : Model -> (Model, Effects.Effects Action)
noFx model =
  ( model, Effects.none)


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Input inputs -> noFx (updateModel model inputs)


updateModel : Model -> Inputs -> Model
updateModel model inputs =
  updateBlobPositionWithKeys model inputs
    |> detectCollisions


updateBlobPositionWithKeys : Model -> Inputs -> Model
updateBlobPositionWithKeys model keys =
  let
    blob = model.blob
    (bx, by) = blob.position
    newX = bx + 10 * (toFloat keys.x)
    newY = by + 10 * (toFloat keys.y)
    newBlob = { blob | position = (newX, newY) }
  in
    { model | blob = newBlob }


detectCollisions : Model -> Model
detectCollisions model =
  let
    (bx, by) = model.blob.position
    distance (fx, fy) = sqrt ((bx-fx)^2 + (by-fy)^2)
    newFood = List.filter (\f -> (distance f.position) > (f.size + model.blob.size)) model.food
    points = (List.length model.food) - (List.length newFood)
  in
    { model
      | food = newFood
      , score = model.score + points
    }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (wx, wy) = model.windowSize
    blob = filled Color.red (circle model.blob.size)
    translatedBlob = move model.blob.position blob
    food = Food.view model.food
  in
    Html.div
          []
          [ Html.fromElement (collage wx wy (translatedBlob :: food))
          , Html.p [] [Html.text ("Score: " ++ (toString model.score))]
          ]


timer : Signal Time.Time
timer = Time.fps 30


inputs : List (Signal Action)
inputs =
  [ Signal.map Input (Signal.map3 Inputs
                        (Signal.map .x Keyboard.wasd)
                        (Signal.map .y Keyboard.wasd)
                        timer)
  ]


app : StartApp.App Model
app =
    start { init = init
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
