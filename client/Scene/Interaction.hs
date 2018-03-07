module Scene.Interaction where

import Common
import Scene.Types
import Types

import Scene.Drawing

import qualified Web.KeyCode as Key
import Scene.Viewport (toLocal, zoomView, panView)
import Scene.Settings (zoomBy, scaleBrush)

import Document (applyEdit, applyCmd')

import Input hiding (init, update)
import Svg
import Miso (class_, View)

import qualified Data.Set as S
import Control.Lens (assign, (.=),  (%=), preview, use)
import Linear

import Debug.Trace

action :: Handler () -> Interaction
action handler = Interaction
  { update = handler
  , cursor = ("auto", False)
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
transition action = do
   modify <- asks snd
   transition' (action & over #update modify)

transition' :: Interaction -> Handler ()
transition' = assign #interaction

runCommand :: DocCmd -> Handler ()
runCommand cmd = do
  doc <- gets (view #document)
  forM_ (applyCmd' cmd doc) $ \((edit, _), doc') -> do
    #document .= doc'
    updateEdit edit
  tell [cmd]

updateEdit :: Edit -> Handler ()
updateEdit = \case
  Add objs          -> #selection .= (fst <$> objs)
  Transform ids _ _ -> #selection .= ids
  Many edits -> mapM_ updateEdit edits
  _ -> return ()


makeEdit :: Edit -> Handler ()
makeEdit edit = runCommand (DocEdit edit)



getId :: Handler ObjId
getId = do
  i <- gets (view #nextId)
  #nextId %= succ
  return i

createObject :: Object -> Handler ObjId
createObject obj = do
  i <- getId
  i <$ makeEdit (Add [(i, obj)])


event :: Cond Input.Event
event = asks snd

withEvent :: (Input.Event -> Cond a) -> Cond a
withEvent f = event >>= f

matchBy :: (Input.Event -> Bool) -> Cond'
matchBy f = withEvent (guard . f)

maybeCond :: Cond (Maybe a) -> Cond a
maybeCond = (>>= Cond . lift)

matches :: (Input.Event -> Maybe a) -> Cond a
matches f = maybeCond $ f <$> event

match :: Traversal' Input.Event a -> Cond a
match t = matches (preview t)

matchesEnv :: (Env -> Maybe a) -> Cond a
matchesEnv f = maybeCond $ asks (f . fst)

matchEnv :: Traversal' Env a -> Cond a
matchEnv t = matchesEnv (preview t)


wheel = match _MouseWheel
mouseMove = match _MouseMove

mouseDown b = matchBy (== MouseDown b)
mouseUp b = matchBy (== MouseUp b)


click b = matchBy (== Click b)


mouseLocal :: Cond Position
mouseLocal = do
  vp <- view (_1 . #viewport)
  toLocal vp <$> mouseMove


pushHandler ::  (Handler () -> Handler ()) -> Handler a -> Handler a
pushHandler f = local (over _2 (. f))

transitionAlternative :: Handler () -> Interaction -> Handler ()
transitionAlternative alt action = transition $
    action & over #update (pushAlternative alt)

pushAlternative alt handler =
    alt <|> pushHandler (pushAlternative alt) handler


until :: Cond () -> Interaction -> Handler ()
until endCondition action = do
   current <- getCurrent
   transitionAlternative (cond endCondition >> commitPending >> transition current) action



while :: Interaction -> (Cond', Cond') -> Handler ()
while action (start, end) = cond start >> until end action


whileButton :: Button -> Interaction -> Handler ()
whileButton b action = while action (mouseDown b, mouseUp b)

whileKey :: Key -> Interaction -> Handler ()
whileKey k action = while action (keyDown k, keyUp k)

mouseHover :: Cond ObjId
mouseHover = matchEnv (#input . #hover . traverse)

hasSelection :: Cond [ObjId]
hasSelection = do
  selection <- view (_1 . #selection)
  guard (not (null selection))
  return selection


isKeyDown :: Key -> Env -> Bool
isKeyDown k = S.member k . view (#input . #keys)

areKeysDown :: [Key] -> Env -> Bool
areKeysDown keys env = all (flip S.member down) keys where
  down = view (#input . #keys) env


keysHeld :: [Key] -> Cond'
keysHeld keys = asks (areKeysDown keys . fst) >>= guard

keyDown k = matchBy (== KeyDown k)
keyUp k = matchBy (== KeyUp k)

mouseDownOn :: Button -> Cond ObjId
mouseDownOn b = mouseDown b >> mouseHover

type Binding = (Key.Key, Set Key.Key)

matchBinding :: [(Binding, a)] -> (Env, Input.Event) -> Maybe a
matchBinding bindings (Env{..}, KeyDown k) = lookup (k, S.delete k $ input ^. #keys) bindings
matchBinding _        _                   = Nothing

keyBinding :: [(Binding, a)] -> Cond a
keyBinding bindings = maybeCond $ asks (matchBinding bindings)


base :: Interaction
base = action update where
  update = do
    pos <- gets localMouse
    pushAlternative cancel $
      whileKey Key.Space drawBoxes <|>
        keys <|>
        mouseSelect pos <|>
        zoomOn pos <|> panOn pos

  traceEvent s = cond (event >>= \e -> traceShow (s, e) (return ()))

  cancel = do
    cond (keyDown Key.Escape <|> void (match _Focus))
    transition base

  delete = do
    selected <- cond hasSelection
    makeEdit (Delete selected)
    #selection .= []

  keys = join $ cond (keyBinding bindings)

  bindings =
    [ ((Key.KeyZ, [Key.Control]), runCommand DocUndo)
    , ((Key.KeyZ, [Key.Control, Key.Shift]), runCommand DocRedo)
    , ((Key.Delete, []), delete)
    ]

  panOn pos = do
    cond_ (mouseDown LeftButton)
    #selection .= []
    until (mouseUp LeftButton) (pan pos)

  zoomOn pos = do
    delta <- cond wheel
    #viewport %= zoomView delta pos


ordNub = S.toList . S.fromList
addSelection obj objs  = ordNub (obj:objs)

mouseSelect :: Position -> Handler ()
mouseSelect pos = do
  objId <- cond (mouseDownOn LeftButton)
  (cond_ (keysHeld [Key.Shift]) >> #selection %= addSelection objId)
      <|> #selection .= [objId]

  env <- get
  until (mouseUp LeftButton) $
    dragObjects (env ^. #selection) (localMouse env)

--
pan :: Position -> Interaction
pan origin = action update & #cursor .~ ("move", True) where
  update = do pos <- cond mouseMove
              #viewport %= panView origin pos
       <|> do delta <- cond wheel
              #viewport %= zoomView delta origin

_brushWidth = #settings . #brushWidth
brush env = (localMouse env, view _brushWidth env)

wheelBrush :: Handler ()
wheelBrush = do
  delta <- cond wheel
  #settings %= scaleBrush delta


drawPoints :: Position -> Interaction
drawPoints pos = action update
  & #cursor .~ ("none", True)
  & #decoration .~ Just (Brush pos)
  where

    update = void wheelBrush
      <|> do cond mouseLocal >>= transition . drawPoints
      <|> do cond (click LeftButton)
             (pos, width) <- gets brush
             void $ createObject (ObjPoint pos width)


drawBoxes :: Interaction
drawBoxes = action update & #cursor .~ ("crosshair", True) where
   update = do
     pos <- gets localMouse
     i <- getId
     whileButton LeftButton (drawBox i pos)


drawBox :: ObjId -> Position -> Interaction
drawBox i origin = drawBox' origin where
  drawBox' pos = action update
    & #cursor   .~ ("crosshair", True)
    & #pending  .~ [Add [(i, ObjBox $ makeBox origin pos)]]

  update = cond mouseLocal >>= transition . drawBox'
  makeBox (V2 x y) (V2 x' y') = Box (V2 (min x x') (min y y')) (V2 (max x x') (max y y'))


-- appendLine :: ObjId -> Interaction
-- appendLine i = init
--     { update = update
--     , cursor = ("none", True)
--     , pending = pending
--     } where
--
--        update = do
--           handleClick LeftButton
--           pos <- gets localMouse
--           void $ createObject (ObjPoint pos width)


dragObjects :: [ObjId] -> Position -> Interaction
dragObjects selection origin = dragObjects' (origin, 1)  where
  dragObjects' (pos, scale) = action update
    & #cursor  .~ (if distance pos origin > 1.0 then "move" else "default", True)
    & #pending .~ [Transform selection scale (pos - origin) | changed (pos, scale)]
    where
      update = do
         delta <- cond wheel
         transition $ dragObjects' (pos, scale * zoomBy delta)
         <|> do
         pos' <- cond mouseLocal
         transition $ dragObjects' (pos', scale)

      changed (pos, scale) = scale /= 1.0 || norm (pos - origin) > 0


renderDecoration :: Env -> Decoration -> View Input.Event
renderDecoration Env{..} = \case
   Brush pos -> circle pos (settings ^. #brushWidth) [class_ "brush"]
