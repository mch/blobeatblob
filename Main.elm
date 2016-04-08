module Main (..) where

import Graphics.Collage exposing (..)
import Time
import Keyboard
import Effects
import Html
import Html.Events
import Signal
import Task
import StartApp exposing (start)
import Food
import Blob
import Json.Decode exposing ((:=), int, object2)

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
  | MouseMove (Int, Int)


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
    MouseMove (x, y) ->
      updateBlobPositionWithMouse model x y
        |> noFx


updateBlobPositionWithMouse : Model -> Int -> Int -> Model
updateBlobPositionWithMouse model x y =
  let
    blob =
      model.blob
           
    (wx, wy) =
      model.windowSize
                    
    newX =
      x - wx // 2
                        
    newY =
      wy // 2 - y

    newBlob =
      { blob | position = (toFloat newX, toFloat newY) }
  in
    { model | blob = newBlob }

      
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


type alias Collidable = { position : (Float, Float), size : Float }

-- Returns a list of pairs of objects that have collided
detectCollisionsForCircles : List Collidable -> List ( Collidable, Collidable )
detectCollisionsForCircles circles =
  let
    distance : ( Float, Float ) -> ( Float, Float ) -> Float
    distance ( x1, y1 ) ( x2, y2 ) =
      sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

    didCollide : Collidable -> Collidable -> List (Collidable, Collidable)
    didCollide c1 c2 =
      if distance c1.position c2.position <= c1.size + c2.size then
        [(c1, c2)]
      else
        []

    checkPairs : List Collidable -> List (Collidable, Collidable)
    checkPairs l =
      case l of
        h :: t -> List.append (List.concatMap (didCollide h) t) (checkPairs t)
        [] -> []
  in
    Debug.log "collisions" (checkPairs circles)

         
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

--    alternaImpl = detectCollisionsForCircles (model.blob :: model.food.particles)
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
      [ Html.Events.on "mousemove"
          (object2 (,) ("clientX" := int) ("clientY" := int))
          (\xy -> Signal.message address (MouseMove xy)) ]
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
