module Scene.Interaction where

import Common
import Scene.Types
import Types

import Scene.Drawing

import qualified Web.KeyCode as Key
import Scene.Viewport (toLocal)
import Scene.Settings (zoomBy)

import Input  (Event(..), Button(..))
import Svg
import Miso (class_, View)

import Linear

init :: Interaction
init = Interaction
  { update = \env e -> []
  , cursor = ("default", False)
  , view = Nothing
  , pending = []
  }


whenCmd :: Monoid m => Bool -> m -> m
whenCmd True cmds = cmds
whenCmd _ _ = mempty


until :: (Input.Event -> Maybe Interaction) -> Interaction -> Interaction
until cond = #update %~ \update env e -> case cond e of
    Just t  -> [transition t]
    Nothing -> mapTransition (until cond) <$> update env e

untilCommit :: (Input.Event -> Maybe Interaction) -> Interaction -> Interaction
untilCommit cond interaction = interaction & #update %~ \update env e -> case cond e of
    Just t  -> [MakeEdit $ Many (interaction ^. #pending), transition t]
    Nothing -> mapTransition (untilCommit cond) <$> update env e


until' :: Input.Event -> Interaction -> Interaction -> Interaction
until' e t = until (\e' -> if e == e' then Just t else Nothing)


mapTransition :: (Interaction -> Interaction) -> Command -> Command
mapTransition f = \case
  Interact i -> Interact (f i)
  cmd        -> cmd


ignored :: [Command]
ignored = []

transition :: Interaction -> Command
transition = Interact


createObject :: Env -> Object -> Command
createObject Env{..} obj = MakeEdit (Add nextId obj)


base :: Interaction
base = init {update = update} where

  update env = \case
      MouseWheel delta     -> [Zoom delta (localMouse env)]
      MouseDown LeftButton -> [transition' (MouseUp LeftButton) $ pan (localMouse env)]
      _           -> ignored

  transition' e = transition . until' e base




pan :: Position -> Interaction
pan pos = init
  { update = \_ cmd -> case cmd of
      MouseMove mouse  -> [Pan pos mouse]
      MouseWheel delta -> [Zoom delta pos]
      _           -> ignored

  , cursor = ("move", True)
  }



drawPoints ::  Position -> Interaction
drawPoints pos = init
    { update = \env@Env{..} -> \case
        MouseMove mouse   -> [transition $ drawPoints (toLocal viewport mouse)]
        MouseWheel delta  -> [ScaleBrush delta]
        Click LeftButton  -> [createObject env (ObjPoint pos (settings ^. #brushWidth))]

        _         -> ignored
      , view = Just (Brush pos)
      , cursor = ("none", True)
      }


dragSelected :: [ObjId] -> Position -> Interaction
dragSelected selection origin = dragObjects' (origin, 1)  where
  dragObjects' (position, scale) = init
    { update = \Env {..} -> \case
          MouseMove mouse   -> [transition $ dragObjects' (toLocal viewport mouse, scale)]
          MouseWheel delta  -> [transition $ dragObjects' (position, scale * zoomBy delta)]
          _           -> ignored
    , cursor = (if distance position origin > 1.0 then "move" else "default", True)
    , pending = [Transform selection scale (position - origin)]
    }

drawView :: Env -> EditView -> View [Command]
drawView Env{..} = \case
   Brush pos -> circle pos (settings ^. #brushWidth) [class_ "brush"]
