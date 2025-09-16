{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Data.ByteString.Char8 (ByteString)
import Data.Function ((&))
import Lens.Micro (toListOf, (^?))
import Lens.Micro.Aeson (key, values, _Bool)
import Network.HTTP.Client.Conduit (throwErrorStatusCodes)
import Network.HTTP.Conduit (checkResponse)
import Network.HTTP.Simple
import Options.Applicative
import Prettyprinter (fillSep, pretty, vsep)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (stderr)
import Text.Printf (hPrintf, printf)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optUser :: String
  , optToken :: Maybe String
  , optPageSize :: Int
  , optOutput :: FilePath
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
                strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
                    <> value "/dev/stdout"
                    <> showDefaultWith id
              optUser <-
                strArgument $
                  help "The name of the GitHub user"
                    <> metavar "USERNAME"
              pure Options {..}
          )
          ( fullDesc
              <> header "Query the GitHub repositories of a user"
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

  hPrintf stderr "Querying repositories for %s\n" optUser

  let
    apiRequest :: ByteString -> Query -> Request
    apiRequest path query =
      defaultRequest
        & setRequestSecure True
        & setRequestPort 443
        & setRequestHost "api.github.com"
        & setRequestBearerAuth (BS.pack token)
        & addRequestHeader "X-GitHub-Api-Version" "2022-11-28"
        & addRequestHeader "User-Agent" "neilmayhew"
        & setRequestPath path
        & setRequestQueryString query

    fetchRepos :: Int -> IO Yaml.Value
    fetchRepos page = do
      let request =
            apiRequest
              ("/users/" <> BS.pack optUser <> "/repos")
              [ ("type", Just "owner")
              , ("per_page", Just . BS.pack $ show optPageSize)
              , ("page", Just . BS.pack $ show page)
              ]
      getResponseBody <$> httpJSON request {checkResponse = throwErrorStatusCodes}

  repos <-
    getPagedItems
      "repos"
      (const $ Just maxBound)
      (toListOf values)
      fetchRepos

  BS.writeFile optOutput . Yaml.encode $
    let isFork v = or $ v ^? key "fork" . _Bool
    in filter (not . isFork) repos

getPagedItems
  :: String
  -> (a -> Maybe Int)
  -> (a -> [b])
  -> (Int -> IO a)
  -> IO [b]
getPagedItems name totalF itemsF fetcher = go 1 0 Nothing
 where
  go i n mt = do
    let progress (show -> t) = printf "%*d/%s" (length t) n t :: String
    hPrintf stderr "Fetching %s ... %s\n" name (maybe "" progress mt)
    page <- fetcher i
    let total = page & totalF
        items = page & itemsF
        n' = n + length items
    case (total, items) of
      (Just t', _ : _) | n' < t' -> (items ++) <$> go (succ i) n' (Just t')
      _ -> do
        hPrintf stderr "Fetched %d %s\n" n' name
        pure items
