module Scene
  ( module Scene
  , Scene(..)
  , Action(..)
  ) where

import Common

import Miso (View, Attribute, on, onWithOptions, div_, class_, classList_, Options(..))
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

data Action
  = Resize Box
  | Run Command
  | Input Input.Event
    deriving (Show, Eq, Generic)




init :: Scene
init = Scene
  { settings = Settings.init
  , viewport = Viewport.init
  , input = Input.init
  , editor = Nothing
  }

makeEditor :: Image -> (DocName, Document) -> ClientId -> Editor
makeEditor background (docName, doc) clientId = Editor {..} where
    nextId     = i
    selection  = []
    interaction = I.base
    i = succ $ fromMaybe 0 (maxId doc)



setEditor :: Editor -> Scene -> Scene
setEditor e = (#editor .~ Just e) . (#viewport . #size .~ toSize dim) where
    toSize (w, h) = V2 (fromIntegral w) (fromIntegral h)
    dim = e ^. #background . #size

resizeView :: Box -> Scene -> Scene
resizeView b = #viewport . #bounds .~ b


updateInput :: Input.Event -> Scene -> Scene
updateInput e scene = scene & #input %~ Input.update e

interact :: Input.Event -> Scene -> [Command]
interact e scene = case maybeEnv scene of
  Nothing   -> []
  Just env  ->  interact' env (env ^. #interaction)
                -- <> checkBindings env (keys env) e
    where
      interact' env Interaction{..} = update env e

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


runCommand :: Command -> Scene -> Scene
runCommand cmd = over _Env $ case cmd of
   Select ids        -> #selection .~ ids
   Pan pos page      -> #viewport %~ Viewport.panView pos page
   Zoom amount pos   -> #viewport %~ Viewport.zoomView amount pos

   ScaleBrush amount -> #settings %~ Settings.scaleBrush amount
   MakeEdit edit -> (#doc %~ applyEdit edit) . (#nextId %~ succ)

   Interact i -> #interaction .~ i




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
applyPending Env{..} = foldr applyEdit doc pending where
  pending = fromMaybe [] (interaction ^? #pending)




view :: Scene -> View [Command]
view scene = case maybeEnv scene of
  Nothing  -> div_ [] []
  Just env@Env{..} -> div_ [class_ "expand"] $ pure $
    Viewport.view (scene ^. #viewport)
      [ svgImage background
      , drawInteraction env
      , g_ [] (viewObject env <$> M.toList (doc' ^. #instances))
      ]

      where
        doc' = applyPending env

        drawInteraction env = maybeSvg view (I.drawView env)
            where view = env ^. #interaction . #view





viewObject :: Env -> (ObjId, Object) -> View [Command]
viewObject env@Env{..} (objId, object) = case object of
  ObjPoint p r -> circle p r attrs
  ObjBox _ -> error "not implemented"

  where
    attrs =
      [ classList_[ ("object", True), ("selected", elem objId selection)]
      -- , Input.on' "mousedown" Input.buttonDecoder (\b -> if b == LeftButton then actions else [])
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
