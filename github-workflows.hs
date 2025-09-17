{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (unless, when)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro (anyOf, filtered, toListOf, traversed, (^..), (^?))
import Lens.Micro.Aeson (key, values, _Bool, _Integral, _String)
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

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optUser :: String
  , optToken :: Maybe String
  , optPageSize :: Int
  , optOutput :: Maybe FilePath
  , optVerbose :: Bool
  , optNoop :: Bool
  }
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
              optOutput <-
                optional . strOption $
                  help "Write workflows to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
              optVerbose <-
                switch $
                  help "Output progress messages"
                    <> short 'v'
                    <> long "verbose"
              optNoop <-
                switch $
                  help "Don't make any modifications"
                    <> short 'n'
                    <> long "noop"
              optUser <-
                strArgument $
                  help "The name of the GitHub user"
                    <> metavar "USERNAME"
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

  trace $ printf "Querying repositories for %s" optUser

  let
    fetchRepos :: Int -> IO Yaml.Value
    fetchRepos page = do
      let request =
            pathRequest
              ("/users/" <> BS.pack optUser <> "/repos")
              [ ("type", Just "owner")
              , ("per_page", Just . BS.pack $ show optPageSize)
              , ("page", Just . BS.pack $ show page)
              ]
      getResponseBody <$> httpJSON request {checkResponse = throwErrorStatusCodes}

  repos <-
    getPagedItems
      "repos"
      (const Nothing)
      (toListOf values)
      fetchRepos

  let
    isFork v = or $ v ^? key "fork" . _Bool
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
    fmap concat . for (sourceRepos ^.. traversed . key "full_name" . _String) $ \name -> do
      getPagedItems
        ("workflows for " <> T.unpack name)
        (preview $ key "total_count" . _Integral)
        (toListOf $ key "workflows" . values)
        (fetchWorkflows name)

  for_ optOutput $ \fp ->
    BS.writeFile fp . Yaml.encode $ workflows

  let
    isDisabled = anyOf (key "state" . _String) (== "disabled_inactivity")
    disabledWorkflowUrls = workflows ^.. traversed . filtered isDisabled . key "url" . _String

    enableWorkflow :: Text -> IO ()
    enableWorkflow url = do
      let request = urlRequest ("PUT " <> T.unpack url <> "/enable")
      getResponseBody <$> httpNoBody request {checkResponse = throwErrorStatusCodes}

  for_ disabledWorkflowUrls $ \url -> do
    hPrintf stderr "Enabling %s\n" url
    unless optNoop $
      enableWorkflow url
