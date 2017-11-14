module Widgets exposing (..)

import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens)
import Html exposing (Html)

--
-- import Bootstrap.Accordion as Accordion

type alias Update msg model = msg -> model -> (model, Cmd msg)

update : Prism msg msgC -> Lens model modelC -> Update msgC modelC -> Update msg model
update prism lens update msg model = case prism.getOption msg of
  Nothing   -> (model, Cmd.none)
  Just msgC -> let (m, c) = update msgC (lens.get model) in (lens.set m model, Cmd.map prism.reverseGet c)


-- type Update = Update (Widget -> (Widget, Cmd Update))
--
-- type alias Widget = {
--
--   view : () -> Html Update
-- }
--
--
-- -- (Accordion.Config Update -> Accordion.Config Update)


-- accordion : Prism msg msgChild -> Lens model modelChild -> (Accordion.Config model -> Accordion.Config model) -> Widget msg model
-- accordion prism lens config = let
