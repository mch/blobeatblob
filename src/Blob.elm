module Blob (Blob, view) where

import Graphics.Collage exposing (..)
import Color


type alias Blob =
  { position : ( Float, Float )
  , size : Float
  }


view : Blob -> Form
view b =
  move b.position (filled Color.darkBlue (circle b.size))
