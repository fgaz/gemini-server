module Network.Gemini.Server (
  Request
, Response(..)
, Handler
, runServer
, okGemini
, okPlain
, redirect
) where

import Network.Socket (HostName, ServiceName, close)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Network.Run.TCP (runTCPServer)
import Network.URI (URI(URI), parseURI, uriToString)

import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 (toString)

import Data.String (fromString)

import Control.Exception (SomeException, try)

import System.Log.Logger
  ( updateGlobalLogger, setLevel, Priority(INFO)
  , infoM, errorM )

--MAYBE switch to a more modern/efficient uri library
--TODO add client cert
type Request = URI

data Response = Response
  { responseStatus :: Int
  , responseMeta :: String
  , responseBody :: LBS.ByteString }

type Handler = Request -> IO Response

renderHeader :: Int -> String -> LBS.ByteString
renderHeader status meta =
  fromString (show status) <>
  fromString " " <>
  fromString meta <>
  fromString "\CR\LF"

runServer :: Maybe HostName -> ServiceName -> (Request -> IO Response) -> IO ()
runServer host service handler = do
  updateGlobalLogger "Network.Gemini.Server" $ setLevel INFO
  runTCPServer host service talk -- MAYBE server config
  where
    talk s = do --TODO timeouts on send and receive (and maybe on handler)
      msg <- toString <$> recv s 1025 -- 1024 + CR or LF
      -- It makes sense to be very lenient here
      let mURI = parseURI $ takeWhile (not . (`elem` ['\CR', '\LF'])) msg
      case mURI of
        Nothing -> do
          infoM "Network.Gemini.Server" $ show msg <> " 59"
          sendAll s $ renderHeader 59 $ fromString "Invalid URL"
          close s
        Just uri@(URI "gemini:" _ _ _ _) -> do
          response <- try $ handler uri
          case response of
            Right (Response status meta body) -> do
              infoM "Network.Gemini.Server" $ unwords
                                            [ show uri
                                            , show status
                                            , show meta ]
              sendAll s $ renderHeader status meta
              sendAll s body
            Left e -> do
              errorM "Network.Gemini.Server" $ unwords
                                             [ show uri
                                             , "42"
                                             -- double show to escape the string
                                             , show $ show (e :: SomeException) ]
              sendAll s $ renderHeader 42 $ fromString "Internal server error"
        Just uri@(URI scheme _ _ _ _) -> do
              infoM "Network.Gemini.Server" $ show uri <> " 59"
              sendAll s $ renderHeader 59 $ fromString $ "Invalid scheme: " <> scheme

-- | Shorthand for @Response 20 "text/gemini"@
okGemini :: LBS.ByteString -> Response
okGemini = Response 20 $ fromString "text/gemini"

-- | Shorthand for @Response 20 "text/plain"@
okPlain :: LBS.ByteString -> Response
okPlain = Response 20 $ fromString "text/plain"

redirect :: URI -> Response
redirect uri = Response 30 (uriToString id uri "") mempty

