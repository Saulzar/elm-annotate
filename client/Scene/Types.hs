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

import Control.Monad.RWS

import Miso (View)

-- data Command
--   = Select [ObjId]
--   | Pan Position Position
--   | Zoom Float Position
--   | ScaleBrush Float
--   | MakeEdit Edit
--
--   | Interact Interaction
--
--     deriving (Eq, Show, Generic)

data Decoration = Brush Position deriving (Eq, Show, Generic)
newtype Handler a = Handler { unHandler :: RWST Input.Event [Edit] Env Maybe a }
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative
    , MonadState Env, MonadReader Input.Event, MonadWriter [Edit])


runHandler :: Handler a -> Input.Event -> Env -> (Env, [Edit])
runHandler (Handler h) e env = fromMaybe (env, []) (execRWST h e env)


data Interaction = Interaction
  { update      :: Handler ()
  , decoration  :: Maybe Decoration
  , cursor      :: (MisoString, Bool)
  , pending     :: [Edit]
  } deriving (Generic)

instance Eq Interaction where
  i == i' =  i ^. #decoration == i' ^. #decoration
          && i ^. #pending == i' ^. #pending

instance Show Interaction where
  show i = "Interaction"

data Image = Image
  { size     :: (Int, Int)
  , source   :: MisoString
  } deriving (Generic, Show, Eq)


data Editor = Editor
  { document  :: Document
  , name      :: DocName
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

  , document  :: Document
  , name      :: DocName
  , nextId      :: ObjId
  , selection   :: [ObjId]
  , interaction :: Interaction

  , background  :: Image
  , input       :: Input.State
  } deriving (Generic, Show, Eq)


localMouse :: Env -> Position
localMouse Env{..} = toLocal viewport (input ^. #mouse)
