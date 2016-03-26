import Graphics.Collage exposing (..)
import Color
import Effects
import Html
import Signal
import Task
import StartApp exposing (start)

type alias Model =
  {}

type Action
  = Tick
    
init : (Model, Effects.Effects Action)
init =
  ({}, Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  (model, Effects.none)
    
view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.text "Hello World"
    
app =
    start { init = init
          , update = update
          , view = view
          , inputs = []
          }

main =
    app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
