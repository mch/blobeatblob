import Graphics.Collage exposing (..)
import Color
import Window

import Effects
import Html
import Signal
import Task
import StartApp exposing (start)


type alias Model =
  { windowSize : (Int, Int) }

    
type Action
  = WindowSize (Int, Int)

    
init : (Model, Effects.Effects Action)
init =
  ({ windowSize = (800, 600) }, Effects.none)

    
update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    WindowSize (wx, wy) -> ( { model | windowSize = (wx, wy) }, Effects.none)

                           
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (wx, wy) = model.windowSize
  in
    Html.fromElement (collage wx wy [filled Color.red (circle 50)])

      
app =
    start { init = init
          , update = update
          , view = view
          , inputs = [Signal.map (\x -> WindowSize x) Window.dimensions]
          }

      
main =
    app.html

       
port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
