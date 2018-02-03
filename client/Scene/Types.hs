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
  | MakeEdit Edit

  | Interact Interaction

    deriving (Eq, Show, Generic)


data EditView = Brush Position deriving (Eq, Show, Generic)


newtype Handler a = Handler { handle :: Input.Event -> Maybe a }

instance Applicative Handler where
  pure = Handler . const . Just
  Handler h <*> Handler h' = Handler $ \e -> h e <*> h' e


instance Alternative Handler where
  empty = Handler (const Nothing)
  Handler h <|> Handler h' = Handler $ \e -> h e <|> h' e

instance Functor Handler where
  fmap f (Handler h) = Handler $ fmap (fmap f) h

data Interaction = Interaction
  { update  :: Env -> Handler [Command]
  , editView :: Maybe EditView
  , cursor  :: (MisoString, Bool)
  , pending :: [Edit]
  } deriving (Generic)

instance Eq Interaction where
  i == i' =  i ^. #editView == i' ^. #editView
          && i ^. #pending == i' ^. #pending

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
  , interaction :: Interaction
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
  , interaction :: Interaction

  , background  :: Image
  , input       :: Input.State
  } deriving (Generic, Show, Eq)


localMouse :: Env -> Position
localMouse Env{..} = toLocal viewport (input ^. #mouse)
