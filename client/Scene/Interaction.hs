module Scene.Interaction where

import Common
import Scene.Types
import Scene.Drawing

import qualified Web.KeyCode as Key
import Scene.Viewport (toLocal)

import Input  (Event(..), Button(..))
import Svg


init :: Interaction
init = Interaction
  { update = \env e -> end
  , cursor = "default"
  , view = const (g_ [] [])
  , pending = []
  }

stateful :: ((a -> Command) -> a -> Interaction) -> a -> Interaction
stateful f  = f set where
  set state = Interact . Just $ f set state

whenCmd :: Monoid m => Bool -> m -> m
whenCmd True cmds = cmds
whenCmd _ _ = mempty

endCmd = Interact Nothing

end :: [Command]
end = [endCmd]

endWith :: [Command] -> [Command]
endWith = (endCmd :)

ignored :: [Command]
ignored = []

start :: Interaction -> Command
start = Interact . Just

pan :: Position -> Interaction
pan = stateful $ \set pos -> init
  { update = \_ cmd -> case cmd of
      MouseMove mouse  -> [Pan pos mouse]
      MouseWheel delta -> [Zoom delta pos]
      MouseUp b   -> whenCmd (b == LeftButton) end

      _           -> ignored

  , cursor = "move"
  }



drawPoints :: Key.Key -> Position -> Interaction
drawPoints key = stateful $ \set pos -> init
  { update = \Env{..} cmd -> case cmd of
      MouseMove mouse   -> [set (toLocal viewport mouse)]
      MouseWheel delta  -> [ScaleBrush delta]
      --
      -- Click b -> mask (b == LeftButton) $ endWith $
      --   createObject scene (Point pos ({position = pos, radius = settings ^. #brushWidth}))

      KeyUp k   -> whenCmd (k == key) end
      _         -> ignored
    , view = \Env{..} -> circle pos (settings ^. #brushWidth) []
    , cursor = "none"
    }
