module Scene.Types
  ( module Scene.Types
  , module Types
  , Viewport (..)
  , Settings (..)
  ) where

import Common

import Types
import qualified Input as Input

import Scene.Viewport (Viewport)
import Scene.Settings (Settings)


import Miso (View)

data Command
  = Select [ObjId]
  | Pan Position Position
  | Zoom Float Position
  | ScaleBrush Float
  | MakeEdit DocName Edit

  | Start Interaction
  | End

    deriving (Eq, Show, Generic)



data Interaction = Interaction
  { update  :: Input.Event -> Env -> [Command]
  , view    :: Env -> View ()
  , cursor  :: String
  , pending :: [Edit]
  } deriving (Generic)

instance Eq Interaction where
  (==) i i' = False

instance Show Interaction where
  show i = "Interaction"

data Image = Image
  { size     :: (Int, Int)
  , source   :: FilePath
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