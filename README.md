# snaplet-bloodhound
A boilerplate [snaplet](https://github.com/snapframework/snap) to conveniently
allow [bloodhound](https://github.com/bitemyapp/bloodhound) [ElasticSearch](https://www.elastic.co/products/elasticsearch)
functions to be lifted into snap web handlers.

## Usage
1. Add the `BloodhoundEnv` to your apps core state:

  ```haskell
    import Snap.Snaplet.Bloodhound
    data App = App
      {...
      ,_bloodhoundEnv :: Snaplet BloodhoundEnv
      ,...
      }
  ```
2. Derive (or manually create) lenses:

  ```haskell
  makeLenses ''App
  ```
3. Implement a `HasBHEnv` instance:

  ```haskell
    instance HasBHEnv App where
      bhEnv = bloodhoundEnv . bhEnv
  ```
4. In your initialisation function, initialise like so, providing a value for
  `server` in the `devel.cfg` file found under the corresponding snaplet directory `snaplets/bloodhound/devel.cfg`.

  ```haskell
    app :: SnapletInit App App
    app = makeSnaplet "app" "Your application" Nothing $ do
      ...
      bloodhoundSnaplet <- nestSnaplet "bloodhound" bloodhoundEnv bloodhoundEnvInit
      ...
      return $ App ... bloodhoundSnaplet ...
  ```
5. You should find `Handler b App` now has a `MonadBH` instance, and so bloodhound functions that require one can be called
   in your handler. E.G:

  ```haskell
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
  ```

