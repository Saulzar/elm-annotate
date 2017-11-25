module Scene.Types exposing (..)


import Image exposing (Image)
import Input

import TypedSvg.Core exposing (Svg)

import Scene.View as View exposing (Geometry)
import Scene.Settings as Settings exposing (Settings)


import Types exposing (..)


import Vector as V exposing (..)

type Command = Pan Position Position | Zoom Float Position | ZoomBrush Float | MakeEdit Edit



type alias Action =
  { update : (Input.Event, Input.State) -> Scene -> Update
  , view   : Scene -> Svg ()
  , cursor : String
  }

type Update = Continue Action (Maybe Command) | Ignored | End (Maybe Command)

type Active = Inactive | Active Action   -- Maybe synonym to break recursive types





type Msg = Start Action | Run Command | Cancel | Ignore

type alias Scene =
  {  background  : Maybe Image
  ,  view        : View.Geometry
  ,  settings    : Settings
  ,  action      : Active
  ,  doc         : Document

  }
