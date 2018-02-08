module Scene.Interaction where

import Common
import Scene.Types
import Types

import Scene.Drawing

import qualified Web.KeyCode as Key
import Scene.Viewport (toLocal, zoomView, panView)
import Scene.Settings (zoomBy, scaleBrush)

import Document (applyEdit)

import Input hiding (init, update)
import Svg
import Miso (class_, View)

import qualified Data.Set as S
import Control.Lens (assign, (.=),  (%=), preview)
import Linear


init :: Interaction
init = Interaction
  { update = mzero
  , cursor = ("default", False)
  , decoration = Nothing
  , pending = mempty
  }


whenCmd :: Monoid m => Bool -> m -> m
whenCmd True cmds = cmds
whenCmd _ _ = mempty

_pending = #interaction . #pending



getCurrent :: Handler Interaction
getCurrent = gets (view #interaction)

-- until :: (Handler Interaction -> Interaction) -> Handler Interaction
-- until f = do
--   current <- getCurrent
--
--
--       fmap toCmd handleEnd <|> fmap wrapCmd (update env) where
--
--     wrapCmd = fmap (mapTransition (until handleEnd))
--     toCmd t = [pendingEdits interaction, transition t]

-- until' :: Input.Event -> Interaction -> Interaction -> Interaction
-- until' e t = until (\e' -> if e == e' then Just t else Nothing)


-- mapTransition :: (Interaction -> Interaction) -> Command -> Command
-- mapTransition f = \case
--   Interact i -> Interact (f i)
--   cmd        -> cmd

commitPending :: Handler ()
commitPending = gets (view _pending) >>= traverse_ makeEdit

transition :: Interaction -> Handler ()
transition = assign #interaction

makeEdit :: Edit -> Handler ()
makeEdit edit = do
  #document %= applyEdit edit
  tell [edit]




getId :: Handler ObjId
getId = do
  i <- gets (view #nextId)
  #nextId %= succ
  return i

createObject :: Object -> Handler ObjId
createObject obj = do
  i <- getId
  i <$ makeEdit (Add i obj)


matchBy :: (Input.Event -> Bool) -> Handler ()
matchBy f = ask >>= guard . f

matches :: (Input.Event -> Maybe a) -> Handler a
matches f = asks f >>= Handler . lift

match :: Traversal' Input.Event a -> Handler a
match t = matches (preview t)



handleWheel = match _MouseWheel
handleMouseMove = match _MouseMove

handleMouseDown b = matchBy (== MouseDown b)
handleMouseUp b = matchBy (== MouseUp b)


handleClick b = matchBy (== Click b)


handleMouseLocal :: Handler Position
handleMouseLocal = do
  vp <- gets (view #viewport)
  toLocal vp <$> handleMouseMove

until :: Handler () -> (Handler () -> Interaction) -> Handler ()
until cond action = do
   current <- getCurrent
   transition (action $ cond >> commitPending >> transition current)

while :: (Handler () -> Interaction) -> (Handler (), Handler ()) -> Handler ()
while action (start, end) = start >> until end action


whileButton :: Button -> (Handler () -> Interaction) -> Handler ()
whileButton b action = while action (handleMouseDown b, handleMouseUp b)

whileKey :: Key -> (Handler () -> Interaction) -> Handler ()
whileKey k action = while action (matchBy (== KeyDown k), matchBy (== KeyUp k))

mouseOver :: Handler ObjId
mouseOver = gets (view (#input . #over)) >>= Handler . lift

isKeyDown :: Key -> Handler Bool
isKeyDown k = gets (S.member k . view (#input . #keys))

keyDown :: Key -> Handler ()
keyDown k = isKeyDown k >>= guard

mouseDownOn :: Button -> Handler ObjId
mouseDownOn b = handleMouseDown b >> mouseOver


base :: Interaction
base = init { update = update } where
  update = do
    pos <- gets localMouse
    do delta <- handleWheel
       #viewport %= zoomView delta pos
     <|> whileKey Key.Control (drawPoints pos)
     <|> mouseSelect pos

     <|> (do handleMouseDown LeftButton
             #selection .= []
             until (handleMouseUp LeftButton) (pan pos))


ordNub = S.toList . S.fromList

mouseSelect :: Position -> Handler ()
mouseSelect pos =  do
  objId <- mouseDownOn LeftButton
  shift <- isKeyDown Key.Shift

  selection <- gets (view #selection)
  let selection' = if shift then ordNub (objId:selection) else [objId]

  #selection .= selection'
  until (handleMouseUp LeftButton) $
    dragObjects selection' pos

  --            <|> whileButton' LeftButton (pan (localMouse env))
  --            <|> whileKey' Key.Control (drawPoints (localMouse env))
  --
  -- whileButton' = whileButton base
  -- whileKey' = whileKey base

--
pan :: Position -> Handler () -> Interaction
pan origin handleEnd = init {update = update, cursor = ("move", True)} where
  update = handleEnd
    <|> do pos <- handleMouseMove
           #viewport %= panView origin pos
    <|> do delta <- handleWheel
           #viewport %= zoomView delta origin


drawPoints :: Position -> Handler () -> Interaction
drawPoints pos handleEnd = init
  { update = update
  , cursor = ("none", True)
  , decoration = Just (Brush pos)
  } where

    update = handleEnd
      <|> do pos' <- handleMouseLocal
             transition $ drawPoints pos' handleEnd
      <|> do delta <- handleWheel
             #settings %= scaleBrush delta
      <|> do handleClick LeftButton
             pos <- gets localMouse
             width <- gets (view (#settings . #brushWidth))
             void $ createObject (ObjPoint pos width)


dragObjects :: [ObjId] -> Position -> Handler () -> Interaction
dragObjects selection origin handleEnd = dragObjects' (origin, 1)  where
  dragObjects' (pos, scale) = init
    { update = update
    , cursor = (if distance pos origin > 1.0 then "move" else "default", True)
    , pending = [Transform selection scale (pos - origin)]
    } where
      update = handleEnd
        <|> do pos' <- handleMouseLocal
               transition $ dragObjects' (pos', scale)
        <|> do delta <- handleWheel
               transition $ dragObjects' (pos, scale * zoomBy delta)


renderDecoration :: Env -> Decoration -> View Input.Event
renderDecoration Env{..} = \case
   Brush pos -> circle pos (settings ^. #brushWidth) [class_ "brush"]
