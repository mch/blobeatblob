import Graphics.Collage exposing (..)
import Color
import Window
import Mouse
import Time

import Effects
import Html
import Signal
import Task
import StartApp exposing (start)


type alias Model =
  { windowSize : (Int, Int)
  , blobPosition : (Float, Float)
  }


type Action
  = WindowSize (Int, Int)
  | MousePosition (Int, Int)
  | Tick Float


init : (Model, Effects.Effects Action)
init =
  ({ windowSize = (800, 600)
   , blobPosition = (0, 0)
   }
  , Effects.none
  )


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    WindowSize (wx, wy) -> ( { model | windowSize = (wx, wy) }, Effects.none)
    MousePosition mp -> ( updateBlobPosition model mp, Effects.none)
    Tick dt -> ( model, Effects.none)


updateBlobPosition model mousePosition =
  let
    blobSpeed = 0.1
    (wx, wy) = model.windowSize
    (mx, my) = mousePosition
    -- corrected mouse position, mouse position in the collage coordinate system
    (cmx, cmy) = (toFloat (mx - wx // 2), toFloat (wy // 2 - my))
    (bx, by) = model.blobPosition
    (dx, dy) = (cmx - bx, cmy - by)
    newbx = bx + blobSpeed * dx
    newby = by + blobSpeed * dy
  in
    { model | blobPosition = (newbx, newby) }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (wx, wy) = model.windowSize
    blob = filled Color.red (circle 50)
    translatedBlob = move model.blobPosition blob
  in
    Html.fromElement (collage wx wy [translatedBlob])


timer = Time.fps 30

inputs =
  [ Signal.map WindowSize Window.dimensions
  , Signal.sampleOn timer (Signal.map MousePosition Mouse.position)
  , Signal.map Tick timer
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
