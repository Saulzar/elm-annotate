module Scene where

import Viewport
import qualified Input as Input

import Miso (View)
import Types

data Command
  = Select [ObjId]
  | Pan Position Position
  | Zoom Float Position
  | ZoomBrush Float
  | MakeEdit Edit

data Update
  = Continue (Maybe Interaction) (Maybe Command)
  | Ignored
  | End (Maybe Command)

data Interaction = Interaction
  { update  :: Input.Event -> Input.State -> Scene -> Update
  , view    :: Scene -> View ()
  , cursor  :: String
  , pending :: [Edit]
  }


data Editor = Editor
  { doc       :: Document
  , nextId    :: ObjId
  , selection :: [ObjId]
  , action    :: Maybe Interaction
  , background :: FilePath
  }


data Settings = Settings
  { brushWidth :: Int
  }

data Scene = Scene
  { settings  :: Settings
  , viewport  :: Viewport
  , editor    :: Maybe Editor
  }
