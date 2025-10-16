{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (unless, when)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (fold, for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime, nominalDay)
import Data.Traversable (for)
import Lens.Micro (anyOf, at, filtered, non, toListOf, traversed, (.~), (^.), (^..), (^?))
import Lens.Micro.Aeson (key, values, _Bool, _Integral, _JSON, _String)
import Lens.Micro.Extras (preview)
import Network.HTTP.Client.Conduit (throwErrorStatusCodes)
import Network.HTTP.Conduit (checkResponse)
import Network.HTTP.Simple
import Options.Applicative
import Prettyprinter (fillSep, pretty, vsep)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf, printf)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Aeson as Lens
import qualified System.Console.Terminal.Size as TS

atKey :: Lens.AsValue t => Aeson.Key -> Lens.Traversal' t (Maybe Aeson.Value)
atKey k = Lens._Object . at k

data Options = Options
  { optToken :: Maybe String
  , optPageSize :: Int
  , optVerbose :: Bool
  , optObscure :: Bool
  , optNoop :: Bool
  , optSaveRepos :: Maybe FilePath
  , optCommand :: Command
  }
  deriving (Show)

data Command
  = Export FilePath
  | Reenable (Maybe Int)
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optToken <-
                optional . strOption $
                  help "The access token to be used for the Bearer authentication"
                    <> short 't'
                    <> long "token"
                    <> metavar "SECRET"
              optPageSize <-
                option auto $
                  help "The page size to be used when using the GitHub API"
                    <> short 'p'
                    <> long "page-size"
                    <> metavar "INT"
                    <> value 20
                    <> showDefault
              optVerbose <-
                switch $
                  help "Output progress messages"
                    <> short 'v'
                    <> long "verbose"
              optObscure <-
                switch $
                  help "Obscure private repo names in output messages"
                    <> short 'b'
                    <> long "obscure"
              optNoop <-
                switch $
                  help "Don't make any modifications"
                    <> short 'n'
                    <> long "noop"
              optSaveRepos <-
                optional . strOption $
                  help "Save a copy of the repo metadata to FILE as YAML"
                    <> short 's'
                    <> long "save-repos"
                    <> metavar "FILE"
                    <> hidden
              optCommand <-
                hsubparser . fold $
                  [ command "export" $
                      info
                        ( do
                            output <-
                              strOption $
                                help "Write workflows to FILE as YAML"
                                  <> short 'o'
                                  <> long "output"
                                  <> metavar "FILE"
                            pure $ Export output
                        )
                        (progDesc "Export workflow metadata")
                  , command "reenable" $
                      info
                        ( do
                            days <-
                              optional . option auto $
                                help "Also reenable any workflows that will be disabled within the next DAYS days"
                                  <> short 'w'
                                  <> long "within"
                                  <> metavar "DAYS"
                            pure $ Reenable days
                        )
                        (progDesc "Re-enable any workflows that were disabled due to inactivity")
                  ]
              pure Options {..}
          )
          ( fullDesc
              <> header "Manage the GitHub workflows of a user"
              <> footerDoc
                ( Just . vsep $
                    let wrap = fillSep . map pretty . words
                     in [ wrap
                            "If the token isn't provided via the command line it \
                            \will be taken from the GH_TOKEN environment variable."
                        ]
                )
          )
      )

  envToken <- lookupEnv "GH_TOKEN"

  token <- maybe (die "No authentication token found") pure $ optToken <|> envToken

  let
    trace :: String -> IO ()
    trace s = when optVerbose $ hPutStrLn stderr s

    apiRequest :: Request -> Request
    apiRequest request =
      request
        & setRequestBearerAuth (BS.pack token)
        & addRequestHeader "X-GitHub-Api-Version" "2022-11-28"
        & addRequestHeader "User-Agent" "neilmayhew"

    pathRequest :: ByteString -> Query -> Request
    pathRequest path query =
      apiRequest defaultRequest
        & setRequestSecure True
        & setRequestPort 443
        & setRequestHost "api.github.com"
        & setRequestPath path
        & setRequestQueryString query

    urlRequest :: String -> Request
    urlRequest = apiRequest . parseRequest_

    getPagedItems
      :: String
      -> (a -> Maybe Int)
      -> (a -> [b])
      -> (Int -> IO a)
      -> IO [b]
    getPagedItems name totalF itemsF fetcher = go 1 0 Nothing
     where
      go i n mt = do
        let progress :: Maybe Int -> String
            progress (Just (show -> t)) = printf "%*d/%s" (length t) n t
            progress Nothing = printf "%d" n
        trace $ printf "Fetching %s ... %s" name (progress mt)
        page <- fetcher i
        let total = page & totalF
            items = page & itemsF
            n' = n + length items
        case (total, items) of
          (mt', _ : _)
            | all (n' <) mt' ->
                (items ++) <$> go (succ i) n' mt'
          _ -> do
            when (n' > 0) $
              trace $
                printf "Fetched %d %s" n' name
            pure items

  let
    fetchRepos :: Int -> IO Yaml.Value
    fetchRepos page = do
      let request =
            pathRequest
              "/user/repos"
              [ ("type", Just "owner")
              , ("per_page", Just . BS.pack $ show optPageSize)
              , ("page", Just . BS.pack $ show page)
              ]
      getResponseBody <$> httpJSON request {checkResponse = throwErrorStatusCodes}

  trace "Querying repositories for the current user"

  repos <-
    getPagedItems
      "repos"
      (const Nothing)
      (toListOf values)
      fetchRepos

  for_ optSaveRepos $ \fp ->
    BS.writeFile fp . Yaml.encode $ repos

  let
    isFork = or . preview (key "fork" . _Bool)
    sourceRepos = filter (not . isFork) repos

    fetchWorkflows :: Text -> Int -> IO Yaml.Value
    fetchWorkflows repo page = do
      let request =
            pathRequest
              ("/repos/" <> T.encodeUtf8 repo <> "/actions/workflows")
              [ ("per_page", Just . BS.pack $ show optPageSize)
              , ("page", Just . BS.pack $ show page)
              ]
      getResponseBody <$> httpJSON request {checkResponse = throwErrorStatusCodes}

  workflows <- do
    fmap concat . for sourceRepos $ \repo ->
      fmap concat . for (repo ^.. key "full_name" . _String) $ \name -> do
        let
          isPublic = elem "public" . preview (key "visibility" . _String)
          pushed = repo ^? key "pushed_at"
          obscure s =
            let n = min 2 $ (T.length s - 2) `div` 2
             in T.take n s <> "****" <> T.takeEnd n s
          protectedName =
            if isPublic repo || not optObscure
              then name
              else (repo ^. key "owner" . key "login" . _String) <> "/" <> obscure (repo ^. key "name" . _String)
          addInfo = (atKey "last_pushed" .~ pushed) . (atKey "repo_name" . non "" . _String .~ protectedName)
        getPagedItems
          ("workflows for " <> T.unpack protectedName)
          (preview $ key "total_count" . _Integral)
          (map addInfo . toListOf (key "workflows" . values))
          (fetchWorkflows name)

  case optCommand of
    Export fp -> do
      BS.writeFile fp . Yaml.encode $ workflows
    Reenable mDays -> do
      now <- getCurrentTime
      let
        cutoff days = addUTCTime (fromIntegral (days - 60) * nominalDay) now
        isOld t = any ((t <) . cutoff) mDays
        isExpiring =
          and . sequence
            [ anyOf (key "state" . _String) (== "active")
            , anyOf (key "updated_at" . _JSON) isOld
            , anyOf (key "last_pushed" . _JSON) isOld
            ]
        isDisabled = anyOf (key "state" . _String) (== "disabled_inactivity")
        shouldEnable = or . sequence [isExpiring, isDisabled]
        enableWorkflow url = do
          let request = urlRequest ("PUT " <> T.unpack url <> "/enable")
          getResponseBody <$> httpNoBody request {checkResponse = throwErrorStatusCodes}

      for_ (workflows ^.. traversed . filtered shouldEnable) $ \wf -> do
        for_ (wf ^? key "url" . _String) $ \url -> do
          hPrintf stderr "Enabling \"%s\" in %s\n" (wf ^. key "name" . _String) (wf ^. key "repo_name" . _String)
          unless optNoop $
            enableWorkflow url
