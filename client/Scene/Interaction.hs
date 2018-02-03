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
  { update = const empty
  , cursor = ("default", False)
  , editView = Nothing
  , pending = []
  }


whenCmd :: Monoid m => Bool -> m -> m
whenCmd True cmds = cmds
whenCmd _ _ = mempty


pendingEdits :: Interaction -> Command
pendingEdits = MakeEdit . Many . view #pending

until :: Handler Interaction -> Interaction -> Interaction
until handleEnd interaction = interaction & #update %~ \update env ->
      fmap toCmd handleEnd <|> fmap wrapCmd (update env) where

    wrapCmd = fmap (mapTransition (until handleEnd))
    toCmd t = [pendingEdits interaction, transition t]

-- until' :: Input.Event -> Interaction -> Interaction -> Interaction
-- until' e t = until (\e' -> if e == e' then Just t else Nothing)


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

handleWheel :: (Float -> a) -> Handler a
handleWheel f = Handler $ \case
  MouseWheel delta -> Just (f delta)
  _ -> Nothing

handleKeyDown :: (Key.Key -> a) -> Handler a
handleKeyDown f = Handler $ \case
  KeyDown k -> Just (f k)
  _ -> Nothing

handleKeyUp :: (Key.Key -> a) -> Handler a
handleKeyUp f = Handler $ \case
  KeyUp k -> Just (f k)
  _ -> Nothing


handleMouseMove :: (Position -> a) -> Handler a
handleMouseMove f = Handler $ \case
  MouseMove pos -> Just (f pos)
  _ -> Nothing

handleMouseLocal :: Env -> (Position -> a) -> Handler a
handleMouseLocal env f = handleMouseMove (f . toLocal (env ^. #viewport))


handleEvent :: Input.Event -> a -> Handler a
handleEvent e a = Handler $ \e' ->
  if e == e' then Just a else Nothing

whileEvent :: Interaction -> (Input.Event, Input.Event) ->  Interaction -> Handler [Command]
whileEvent self (start, end) dest = handleEvent start [transition dest'] where
  dest' = until (handleEvent end self) dest

whileButton :: Interaction -> Button ->  Interaction -> Handler [Command]
whileButton self b = whileEvent self (MouseDown b, MouseUp b)

whileKey :: Interaction -> Key.Key ->  Interaction -> Handler [Command]
whileKey self b = whileEvent self (KeyDown b, KeyUp b)



 -- type Binding = (Key.Key, [Key.Key])
 --
 --  handleKeys  :: Env -> [(Binding, Command)] -> Handler [Command]
 --  handleKeys Env{..} bindings = \case
  --   Input.KeyDown k   -> maybeToList $ lookup (k, Set.toList (Set.delete k $ input ^. #keys)) bindings
  --   _           -> []


base :: Interaction
base = init {update = update} where

  update env = handleWheel (\delta -> [Zoom delta (localMouse env)])
             <|> whileButton' LeftButton (pan (localMouse env))
             <|> whileKey' Key.Control (drawPoints (localMouse env))

  whileButton' = whileButton base
  whileKey' = whileKey base


pan :: Position -> Interaction
pan origin = init
  { update = const (handleMouseMove (\mouse -> [Pan origin mouse])
                   <|> handleWheel (\delta -> [Zoom delta origin]))

  , cursor = ("move", True)
  }



drawPoints ::  Position -> Interaction
drawPoints pos = init
    { update = \env ->
            handleMouseLocal env (\mouse -> [transition $ drawPoints mouse])
        <|> handleWheel (\delta -> [ScaleBrush delta])
        <|> handleEvent (Click LeftButton) [createObject env (ObjPoint pos (env ^. #settings . #brushWidth))]

      , editView = Just (Brush pos)
      , cursor = ("none", True)
      }
--
--
-- dragSelected :: [ObjId] -> Position -> Interaction
-- dragSelected selection origin = dragObjects' (origin, 1)  where
--   dragObjects' (position, scale) = init
--     { update = \Env {..} -> \case
--           MouseMove mouse   -> [transition $ dragObjects' (toLocal viewport mouse, scale)]
--           MouseWheel delta  -> [transition $ dragObjects' (position, scale * zoomBy delta)]
--           _           -> ignored
--     , cursor = (if distance position origin > 1.0 then "move" else "default", True)
--     , pending = [Transform selection scale (position - origin)]
--     }

drawView :: Env -> EditView -> View [Command]
drawView Env{..} = \case
   Brush pos -> circle pos (settings ^. #brushWidth) [class_ "brush"]
