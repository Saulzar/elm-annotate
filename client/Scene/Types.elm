module Scene.Types exposing (..)


import Image exposing (Image)
import Input

import TypedSvg.Core exposing (Svg)

import Scene.View as View exposing (Geometry)
import Scene.Settings as Settings exposing (Settings)

import Types exposing (..)
import Vector as V exposing (..)

import Common exposing (ObjId)



type Command = Select (List ObjId) | Pan Position Position | Zoom Float Position | ZoomBrush Float | MakeEdit Edit


type alias Action =
  { update : Input.Event -> Input.State -> Scene -> Update
  , view   : Scene -> Svg ()
  , cursor : String
  , pending : List Edit
  }

type Update = Continue (Maybe Action) (Maybe Command) | Ignored | End (Maybe Command)

type Active = Inactive | Active Action   -- Maybe synonym to break recursive types



type Msg = Start Action | Update Action | Run Command | Cancel | Ignore
type alias Msgs = List Msg


type alias Scene =
  { doc       : Document
  , nextId    : ObjId
  , selection : List ObjId
  , action    : Active
  , settings  : Settings
  , view      : Geometry
  , background : Maybe Image
  }
