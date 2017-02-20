{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , OverloadedStrings
           , Rank2Types
           , TemplateHaskell
           , UndecidableInstances
  #-}
{-|
Module     : Snap.Snaplet.Bloodhound
Copyright  : (c) Samuel Yallop
Maintainer : syallop@gmail.com
Stability  : experimental

This module defines a 'Snaplet' for bloodhound's bindings to ElasticSearch for use with the 'Snap' web framework.

To use this module:

1. Add the BloodhoundEnv to your apps core state:

@
  data App = App
    {...
    ,_bloodhoundEnv :: Snaplet BloodhoundEnv
    ,...
    }
@

2. Derive (or manually create) lenses:

@ makeLenses ''App @

3. Implement a 'HasBHEnv' instance:

@
  instance HasBHEnv App where
    bhEnv = bloodhoundEnv . bhEnv
@

4. In your initialisation function, initialise like so, providing a value for
  'server' in the 'devel.cfg' file found under the corresponding snaplet directory 'snaplets/bloodhound/devel.cfg'.

@
  app :: SnapletInit App App
  app = makeSnaplet "app" "Your application" Nothing $ do
    ...
    bloodhoundSnaplet <- nestSnaplet "bloodhound" bloodhoundEnv bloodhoundEnvInit
    ...
    return $ App ... bloodhoundSnaplet ...
@

6. You should find 'Handler b App' now has a 'MonadBH' instance, and so bloodhound functions that require one can be called
   in your handler. E.G:

@
  -- Some bloodhound search function
  searchYourDocuments :: MonadBH m => Search -> m Reply
  searchYourDocuments = searchByType yourIndexName yourMappingName

  -- Can now be used inside your handlers.
  doSomethingWithYourDocs :: Handler App App ()
  doSomethingWithYourDocs = do
    eDoc :: Either EsError (SearchResult YourDocument) <- parseEsResponse =<< searchYourDocuments
    case eDoc of
      Left err
        -> fail . show $ err

      Right (SearchResult _took _timedout _shards (SearchHits _hTotal _hMaxScore hits) _hAggs _hmScrollId)
        -> map (\(Hit _hIndex _hType _hDocId _hScore hSource _hHighlight) -> case hSource of
                   Nothing                 -> ...
                   Just (YourDocument ...) -> ...
               )
               hits
@

-}
module Snap.Snaplet.Bloodhound
  ( BloodhoundEnv
  , bloodhoundEnvInit
  , HasBHEnv
  , bhEnv
  ) where

import Control.Lens
import Control.Monad.State
import Data.Configurator as C
import Data.Maybe
import Database.Bloodhound
import Network.HTTP.Client (newManager,defaultManagerSettings)
import Snap.Snaplet

-- | State required for bloodhound functions: a wrapped 'BHEnv'.
newtype BloodhoundEnv = BloodhoundEnv
  {_bloodhoundEnvBHEnv :: BHEnv
  }
-- Generate a lens into the Bloodhound's bhEnv for convenience.
makeLenses ''BloodhoundEnv


-- | Class of types which have a lens to a BHEnv.
class HasBHEnv a where
  bhEnv :: Lens' a BHEnv

-- | Bloodhound has a lens into its 'BHEnv'.
instance HasBHEnv BloodhoundEnv where
  bhEnv = bloodhoundEnvBHEnv

-- | A Snaplet of a Bloodhound has a lens into it's 'BHEnv'.
instance HasBHEnv (Snaplet BloodhoundEnv) where
  bhEnv = snapletValue . bhEnv

-- | If a Handler's 'v' is an instance of 'HasBHEnv' (Is 'Snaplet
-- Bloodhound'/'Bloodhound'/...), then bloodhound functions can be ran because
-- we can retrieve the BHEnv.
instance HasBHEnv v => MonadBH (Handler b v) where
  getBHEnv = view (snapletValue . bhEnv) <$> getSnapletState

-- | Initialise a 'BloodhoundEnv' Snaplet allowing reading the server from a
-- config file 'devel.cfg' under the snaplets directory.
-- Use a default HTTP manager.
bloodhoundEnvInit :: SnapletInit b BloodhoundEnv
bloodhoundEnvInit = makeSnaplet "bloodhound" "Bloodhound snaplet for ElasticSearch." Nothing $ do
  config <- getSnapletUserConfig

  -- Use a default http manager
  httpManager <- liftIO $ newManager defaultManagerSettings
  esServer    <- liftIO $ Server . fromMaybe "http://127.0.0.1:9200" <$> C.lookup config "server"

  return . BloodhoundEnv . mkBHEnv esServer $ httpManager

