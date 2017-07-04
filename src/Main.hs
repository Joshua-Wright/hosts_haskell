{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main where
import           Control.Concurrent.ParallelIO.Global
import           Control.Exception
import qualified Data.ByteString.Lazy                 as BL
import           Data.Either.Combinators
import           Data.List
import           Data.Maybe
import qualified Data.Set                             as S
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.IO                         as T
import           Network.HTTP.Conduit
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.ProgressBar

hostnamePath :: FilePath
hostnamePath = "/etc/hostname"

makeHeader :: T.Text -> T.Text
makeHeader currentHostname = T.concat
    [ "# hosts file made by j0sh\n"
    , "127.0.0.1 ", currentHostname, " # current hostname for this machine\n"
    , "::1 ", currentHostname, " # current hostname for this machine\n"
    , "127.0.0.1 localhost # IPv4 localhost\n"
    , "::1 localhost #IPv6 localhost\n\n"
    ]

linePrefix :: T.Text
linePrefix = "0.0.0.0 "

data Options = Options
    { urls      :: IO [String]
    , whitelist :: IO [String]
    , hostname  :: IO T.Text
    , output    :: T.Text -> IO ()
    }
defaultOptions  = Options
    { urls      = return []
    , whitelist = getDefaultWhitelist
    , hostname  = getHostName
    , output    = T.putStrLn
    }
readFileLines :: String -> IO [String]
readFileLines path = do
    content <- readFile path
    return (lines content)
outputFile :: String -> (T.Text -> IO ())
outputFile path = T.writeFile path

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['i'] ["input"]
        (ReqArg (\path opts -> opts{urls=(readFileLines path)}) "INPUT")
        "input file"
    , Option ['o'] ["output"]
        (ReqArg (\path opts -> opts{output=(outputFile path)}) "OUTPUT" )
        "output file. Defaults to stdout"
    , Option ['w'] ["whitelist"]
        (ReqArg (\path opts -> opts{whitelist=(readFileLines path)}) "WHITELIST")
        "whitelist file. Defaults to ~/.hosts_whitelist.txt"
    , Option ['h'] ["hostname"]
        (ReqArg (\host opts -> opts{hostname=(return . T.pack $ host)}) "HOSTNAME")
        "hostname to use. Tries to determine system hostname if unspecified"
    ]

getOpts :: [String] -> IO (Options, [String])
getOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hosts [OPTIONS...]"

processArgs :: [String] -> IO ()
processArgs args = do
    (opts, f) <- getOpts args
    whitelist <- whitelist opts
    urls_     <- urls opts
    hostname  <- hostname opts
    let urls = filterComments urls_
    (bar, _)  <- startProgress (msg "Downloading") exact 80 $ fromIntegral $ length urls
    -- we use extraWorkerWhileBlocked to create extra threads to do work while
    -- existing ones are blocking on file downloads
    sets      <- parallel $ map (extraWorkerWhileBlocked . downloadHostsFile bar) urls
    putStrLn ""
    putStrLn "Joining files and writing output..."
    let finalSet     = S.unions sets
        whiteListSet = S.fromList $ map T.pack $ filterComments whitelist
        outputLines  = S.toList $ S.difference finalSet whiteListSet
        header       = makeHeader hostname
    output opts $ T.append header $ T.unlines outputLines

main :: IO ()
main = getArgs >>= processArgs


filterLine :: T.Text -> Maybe T.Text
filterLine (T.stripPrefix "127.0.0.1" -> Just suf) = Just $ addPrefix suf
filterLine (T.stripPrefix "0.0.0.0"   -> Just suf) = Just $ addPrefix suf
filterLine (T.stripPrefix "::1"       -> Just suf) = Just $ addPrefix suf
filterLine _                                       = Nothing
-- remove anything after a # comment and add the starting prefix
addPrefix l = T.append linePrefix $ T.strip $ T.takeWhile (/= '#') l

downloadHostsFile :: ProgressRef -> String -> IO (S.Set T.Text)
downloadHostsFile bar url = do
    fileContent <- catch (simpleHttp url)
                         nopHandler
    incProgress bar 1
    let eitherText = T.decodeUtf8' $ BL.toStrict fileContent
        text = fromRight "" eitherText
        outputSet = S.fromList $ mapMaybe filterLine $ T.lines text
    return outputSet
  where
    nopHandler :: HttpException -> IO BL.ByteString
    nopHandler e = do
      incProgress bar 1
      return ""

filterComments :: [String] -> [String]
filterComments = mapMaybe strFilter
    where
        strFilter :: String -> Maybe String
        strFilter x
            | "#" `isPrefixOf` x = Nothing
            | x == ""            = Nothing
            | otherwise           = Just x

getHostName :: IO T.Text
getHostName = do
    hostnameFileExists <- doesFileExist (hostnamePath :: FilePath)
    if hostnameFileExists then
        fmap T.strip (T.readFile hostnamePath)
    else
        return "localhost"

getDefaultWhitelist :: IO [String]
getDefaultWhitelist = do
    homeFolder <- getHomeDirectory
    let whitelistPath = homeFolder </> (".hosts_whitelist.txt" :: FilePath)
    fileExists <- doesFileExist (whitelistPath :: FilePath)
    if fileExists then do
        filecontents <- readFile whitelistPath
        return (lines filecontents)
    else
        return []
