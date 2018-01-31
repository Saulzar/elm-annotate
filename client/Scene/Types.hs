module Scene.Types
  ( module Scene.Types
  , module Types
  , Viewport (..)
  , Settings (..)
  ) where

import Common

import Types
import qualified Input as Input

import Miso.String (MisoString)

import Scene.Viewport (Viewport, toLocal)
import Scene.Settings (Settings)


import Miso (View)

data Command
  = Select [ObjId]
  | Pan Position Position
  | Zoom Float Position
  | ScaleBrush Float
  | MakeEdit DocName Edit

  | Interact (Maybe Interaction)

    deriving (Eq, Show, Generic)



data Interaction = Interaction
  { update  :: Env -> Input.Event -> [Command]
  , view    :: Env -> View [Command]
  , cursor  :: MisoString
  , pending :: [Edit]
  } deriving (Generic)

instance Eq Interaction where
  (==) i i' = False

instance Show Interaction where
  show i = "Interaction"

data Image = Image
  { size     :: (Int, Int)
  , source   :: MisoString
  } deriving (Generic, Show, Eq)


data Editor = Editor
  { doc       :: Document
  , docName   :: DocName
  , nextId    :: ObjId
  , selection :: [ObjId]
  , interaction :: Maybe Interaction
  , background :: Image
  } deriving (Generic, Show, Eq)

data Scene = Scene
  { settings  :: Settings
  , viewport  :: Viewport
  , input     :: Input.State
  , editor    :: Maybe Editor
  } deriving (Generic, Show, Eq)


data Env = Env
  { settings  :: Settings
  , viewport  :: Viewport

  , doc         :: Document
  , docName     :: DocName
  , nextId      :: ObjId
  , selection   :: [ObjId]
  , interaction :: Maybe Interaction

  , background  :: Image
  , input       :: Input.State
  } deriving (Generic, Show, Eq)


localMouse :: Env -> Position
localMouse Env{..} = toLocal viewport (input ^. #mouse)
