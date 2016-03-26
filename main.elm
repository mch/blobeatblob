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
  , blobPosition : (Int, Int)
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
    (wx, wy) = model.windowSize
    (mx, my) = mousePosition
    (bx, by) = model.blobPosition
  in
    { model | blobPosition = (mx - wx // 2, wy // 2 - my) }

      
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (wx, wy) = model.windowSize
    (bx, by) = model.blobPosition
    blob = filled Color.red (circle 50)
    translatedBlob = move (toFloat bx, toFloat by) blob
  in
    Html.fromElement (collage wx wy [translatedBlob])


inputs =
  [ Signal.map WindowSize Window.dimensions
  , Signal.map MousePosition Mouse.position
  , Signal.map Tick (Time.fps 30)
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
