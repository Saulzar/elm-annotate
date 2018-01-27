{-# LANGUAGE TemplateHaskell #-}

import Types
import Data.Proxy

import Elm.Module
import Elm.TyRep
import Elm.TyRender
import Elm.Json
import Elm.Versions

import Debug.Trace

alterations :: ETypeDef -> ETypeDef
alterations = recAlterType $ \t -> case t of
  ETyApp (ETyApp (ETyCon (ETCon "Map")) k) v      -> checkMap k v
  _ -> t
  where
    tc = ETyCon . ETCon
    -- isComparable (ETyCon (ETCon n)) = n `elem` ["String", "Int"]
    -- isComparable (ETyApp (ETyApp (ETyTuple 2) k1) k2) = isComparable k1 && isComparable k2
    -- isComparable _ = False

    checkMap k v = ETyApp (ETyApp (tc "Dict") k) v
                 -- | otherwise  = ETyApp (tc "List") (ETyApp (ETyApp (ETyTuple 2) k) v)



makeModule' :: String  -- ^ Module name
                         -> [DefineElm]  -- ^ List of definitions to be included in the module
                         -> String
makeModule' moduleName defs = unlines $
    [ moduleHeader Elm0p18 moduleName
    , ""
    , "import Json.Decode"
    , "import Json.Encode exposing (Value)"
    , "-- The following module comes from bartavelle/json-helpers"
    , "import Json.Helpers exposing (..)"
    , "import Dict exposing (Dict)"
    , "import Set exposing (Set)"
    , "import Common exposing (..)"
    , ""
    ] ++ [makeModuleContentWithAlterations (defaultAlterations . alterations) defs]

elmModule :: String
elmModule = makeModule' "Types"
  [ DefineElm (Proxy @ Box)
  , DefineElm (Proxy @ Extents)
  , DefineElm (Proxy @ Edit)
  , DefineElm (Proxy @ Object)
  , DefineElm (Proxy @ Document)

  , DefineElm (Proxy @ ClientMsg)
  , DefineElm (Proxy @ ServerMsg)

  , DefineElm (Proxy @ DocInfo)
  , DefineElm (Proxy @ Config )

  , DefineElm (Proxy @ Dataset)

  ]



main :: IO ()
main = putStrLn elmModule
