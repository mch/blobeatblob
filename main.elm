import Graphics.Collage exposing (..)
import Color
import Window
import Mouse

import Effects
import Html
import Signal
import Task
import StartApp exposing (start)


type alias Model =
  { windowSize : (Int, Int)
  , mousePosition : (Int, Int)
  }

    
type Action
  = WindowSize (Int, Int)
  | MousePosition (Int, Int)

    
init : (Model, Effects.Effects Action)
init =
  ({ windowSize = (800, 600)
   , mousePosition = (0, 0)
   }
  , Effects.none
  )

    
update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    WindowSize (wx, wy) -> ( { model | windowSize = (wx, wy) }, Effects.none)
    MousePosition mp -> ( updateMousePosition model mp, Effects.none)


updateMousePosition model mousePosition =
  let
    (wx, wy) = model.windowSize
    (mx, my) = mousePosition
  in
    { model | mousePosition = (mx - wx // 2, wy // 2 - my) }

      
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (wx, wy) = model.windowSize
    (mx, my) = model.mousePosition
    blob = filled Color.red (circle 50)
    translatedBlob = move (toFloat mx, toFloat my) blob
  in
    Html.fromElement (collage wx wy [translatedBlob])


inputs =
  [ Signal.map (\x -> WindowSize x) Window.dimensions
  , Signal.map (\x -> MousePosition x) Mouse.position
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
