module Scene.Interaction where

import Common
import Scene.Types

import qualified Web.KeyCode as Key

import Input  (Event(..), Button(..))
import Svg


init :: Interaction
init = Interaction
  { update = \e env -> end
  , cursor = "default"
  , view = const (g_ [] [])
  , pending = []
  }

stateful :: ((a -> Interaction) -> a -> Interaction) -> a -> Interaction
stateful f  = f set where
  set state = f set state

whenCmd :: Bool -> [Command] -> [Command]
whenCmd True cmds = cmds
whenCmd _ _ = []

end :: [Command]
end = [End]

ignored :: [Command]
ignored = []

pan :: Position -> Interaction
pan = stateful $ \set pos -> init
  { update = \e env -> case e of
      MouseMove mouse  -> pure (Pan pos mouse)
      MouseWheel delta -> pure (Zoom delta pos)
      MouseUp b   -> whenCmd (b == LeftButton) end

      _           -> ignored

  , cursor = "move"
  }
