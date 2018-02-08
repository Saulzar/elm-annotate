module Scene
  ( module Scene
  , Scene(..)
  ) where

import Common

import Miso (View, Attribute, on, onWithOptions, div_, class_, classList_, Options(..), style_, emptyDecoder)
import qualified Miso.String as S

import qualified Scene.Settings as Settings
import qualified Scene.Viewport as Viewport
import qualified Scene.Interaction as I
import qualified Input

import Scene.Drawing

import qualified Web.KeyCode as Key
import Document (maxId, applyEdit)

import Input (Button(..))
import Scene.Viewport (Viewport, toLocal)

import qualified Data.Set as Set
import qualified Data.Map as M

import Control.Lens (prism', Prism')

import qualified Debug.Trace as Debug
import Types
import Scene.Types
import Svg

import Miso.Html.Property (textProp)

-- import Debug.Trace

init :: Scene
init = Scene
  { settings = Settings.init
  , viewport = Viewport.init
  , input = Input.init
  , editor = Nothing
  }

makeEditor :: Image -> (DocName, Document) -> ClientId -> Editor
makeEditor background (name, document) clientId = Editor {..} where
    nextId     = i
    selection  = []
    interaction = I.base
    i = succ $ fromMaybe 0 (maxId document)



setEditor :: Editor -> Scene -> Scene
setEditor e = (#editor .~ Just e) . (#viewport . #size .~ toSize dim) where
    toSize (w, h) = V2 (fromIntegral w) (fromIntegral h)
    dim = e ^. #background . #size

resizeView :: Box -> Scene -> Scene
resizeView b = #viewport . #bounds .~ b


updateInput :: Input.Event -> Scene -> Scene
updateInput e scene = scene & #input %~ Input.update e



interact :: Input.Event -> Scene -> (Scene, [Edit])
interact e scene = case maybeEnv (updateInput e scene) of
  Nothing   -> (scene, [])
  Just env  -> interact' env (env ^. #interaction . #update)
                -- <> checkBindings env (keys env) e
    where
      interact' env update = over _1 toScene $ runHandler update e env

-- type Binding = (Key.Key, [Key.Key])
--
-- keys :: Env -> [(Binding, Command)]
-- keys env@Env{..} = always <> notInteracting
--
--   where
--     notInteracting = case env ^. #interaction of
--       Nothing -> [whileKey Key.Space $ I.drawPoints (localMouse env)]
--       Just _  -> []
--
--     always =
--       [ (key Key.Escape, I.endCmd)
--       , (key Key.Delete, MakeEdit (Many (Delete <$> selection)))
--       ]
--
--     key k = (k, [])
--     whileKey k interaction = (key k, I.transition $ I.until (== Input.KeyUp k) interaction)

-- checkBindings :: Env -> [(Binding, Command)] -> Input.Event -> [Command]
-- checkBindings Env{..} bindings = \case
--   Input.KeyDown k   -> maybeToList $ lookup (k, Set.toList (Set.delete k $ input ^. #keys)) bindings
--   _           -> []




svgImage :: Image -> View action
svgImage Image{..} = image_ [xlinkHref_ source, x_ 0, y_ 0, width_ w, height_ h ] []
  where (V2 w h) = dimVector size





-- events :: Env -> [Attribute [Command]]
-- events env =
--   [ on "mousedown" Input.buttonDecoder $ \case
--         LeftButton -> [Select [], I.transition (I.pan (localMouse env))]
--         _          -> []
--
--   , on "wheel" Input.wheelDecoder (\delta ->
--         [Zoom delta (localMouse env)])
--
--   , class_ "expand"
--   ]

maybeSvg :: Maybe a -> (a -> View action) -> View action
maybeSvg ma f = maybe (g_ [] []) f ma

applyPending :: Env -> Document
applyPending Env{..} = foldr applyEdit document pending where
  pending = fromMaybe [] (interaction ^? #pending)



render :: Scene -> View Input.Event
render scene = case maybeEnv scene of
  Nothing  -> div_ [] []
  Just env -> renderEnv env

renderEnv env@Env{..} = div_ attrs $ pure $
  Viewport.render viewport
    [ svgImage background
    , renderInteraction env
    , g_ [] (viewObject env <$> M.toList (document' ^. #instances))
    ]

    where
      document' = applyPending env

      renderInteraction env = maybeSvg d (I.renderDecoration env)
          where d = env ^. #interaction . #decoration

      attrs = [
          class_ "expand"
          , style_ [("pointer-events", "auto"), ("cursor", interaction ^. #cursor . _1)]
          , on "mousedown" Input.buttonDecoder Input.MouseDown
        ]


viewObject :: Env -> (ObjId, Object) -> View Input.Event
viewObject env@Env{..} (objId, object) = case object of
  ObjPoint p r -> circle p r attrs
  ObjBox _ -> error "not implemented"

  where
    attrs =
      [ classList_[ ("object", True), ("selected", elem objId selection)]
      , on "mouseover" emptyDecoder (const (Input.MouseOver objId))
      , on "mouseout" emptyDecoder (const (Input.MouseOut objId))
      --, on "click" Input.buttonDecoder (\b -> if b == LeftButton then actions else [])
      ]

    -- actions =
    --   [ Select [objId]
    --   , I.transition (I.untilCommit (== Input.MouseUp LeftButton) $ I.dragSelected [objId] (localMouse env))
    --   ]


maybeEnv :: Scene -> Maybe Env
maybeEnv Scene{..}
  | Just Editor{..} <- editor = Just Env{..}
  | otherwise                 = Nothing

toScene :: Env -> Scene
toScene Env{..} = Scene{..}  where
  editor = Just Editor{..}

_Env :: Prism' Scene Env
_Env = prism' toScene maybeEnv
