module Network.Gemini.Server (
  Request(..)
, Response(..)
, Handler
, runServer
, okGemini
, okPlain
, redirect
) where

import Network.Socket (Socket, HostName, ServiceName, SockAddr, getPeerName)
import OpenSSL (withOpenSSL)
import qualified OpenSSL.Session as SSL
import OpenSSL.Session (SSL)
import OpenSSL.X509 (X509)
import Network.Run.TCP (runTCPServer)
import Network.URI (URI(URI), parseURI, uriToString)

import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.UTF8 (toString)

import Data.String (fromString)

import Control.Exception (SomeException, try, bracket, catch)

import System.Log.Logger
  ( updateGlobalLogger, setLevel, logM, Priority(INFO, ERROR) )


--MAYBE switch to a more modern/efficient uri library

-- | A Gemini client's request
--
-- @since 0.2.0.0
data Request = Request
  { requestURI :: URI
  , requestCert :: Maybe X509 }

-- | A Gemini server's response
--
-- @since 0.1.0.0
data Response = Response
  { responseStatus :: Int
  , responseMeta :: String
  , responseBody :: LBS.ByteString }

-- | A request handler specifies how the server should respond to the clients'
-- requests
--
-- @since 0.1.0.0
type Handler = Request -> IO Response

renderHeader :: Int -> String -> LBS.ByteString
renderHeader status meta =
  fromString (show status) <>
  fromString " " <>
  fromString meta <>
  fromString "\CR\LF"

-- | Start a Gemini server.
--
-- @since 0.2.0.0
runServer :: Maybe HostName
          -> ServiceName
          -> FilePath -- ^ Path to the server certificate
          -> FilePath -- ^ Path to the private key
          -> (Request -> IO Response) -- ^ Request handler
          -> IO ()
runServer host service cert key handler = withOpenSSL $ do
  updateGlobalLogger "Network.Gemini.Server" $ setLevel INFO
  -- MAYBE server config
  sslCtx <- setupSSL cert key
  runTCPServer host service $ \sock -> do
    peer <- getPeerName sock
    catch -- the ssl session may fail
      (bracket
        (acceptSSL sslCtx sock)
        (`SSL.shutdown` SSL.Unidirectional)
        (talk handler peer))
      (\e -> logM "Network.Gemini.Server" ERROR $ show (e :: SomeException))

setupSSL :: FilePath -> FilePath -> IO SSL.SSLContext
setupSSL cert key = do
  sslCtx <- SSL.context
  SSL.contextSetDefaultCiphers sslCtx
  SSL.contextSetCertificateFile sslCtx cert
  SSL.contextSetPrivateKeyFile sslCtx key
  SSL.contextSetVerificationMode sslCtx $ SSL.VerifyPeer False True $
    -- Accept all certificates, since we don't really care about validity wrt CAs
    -- but only about the public key
    Just $ \_ _ -> pure True
  pure sslCtx

acceptSSL :: SSL.SSLContext -> Socket -> IO SSL
acceptSSL sslCtx sock = do
  ssl <- SSL.connection sslCtx sock
  SSL.accept ssl
  pure ssl

talk :: (Request -> IO Response) -> SockAddr -> SSL -> IO ()
talk handler peer s = do --TODO timeouts on send and receive (and maybe on handler)
  msg <- toString <$> SSL.read s 1025 -- 1024 + CR or LF
  -- It makes sense to be very lenient here
  let mURI = parseURI $ takeWhile (not . (`elem` ['\CR', '\LF'])) msg
  case mURI of
    Nothing -> do
      logRequest INFO peer (Left msg) 59 Nothing
      SSL.lazyWrite s $ renderHeader 59 $ fromString "Invalid URL"
    Just uri@(URI "gemini:" _ _ _ _) -> do
      clientCert <- SSL.getPeerCertificate s
      response <- try $ handler $ Request uri clientCert
      case response of
        Right (Response status meta body) -> do
          logRequest INFO peer (Right uri) status $ Just meta
          SSL.lazyWrite s $ renderHeader status meta
          SSL.lazyWrite s body
        Left e -> do
          logRequest ERROR peer (Right uri) 42 $ Just $ show (e :: SomeException)
          SSL.lazyWrite s $ renderHeader 42 $ fromString "Internal server error"
    Just uri@(URI scheme _ _ _ _) -> do
          logRequest INFO peer (Right uri) 59 Nothing
          SSL.lazyWrite s $ renderHeader 59 $ fromString $ "Invalid scheme: " <> scheme

logRequest :: Priority -> SockAddr -> Either String URI -> Int -> Maybe String -> IO ()
logRequest p peer uri code meta = logM "Network.Gemini.Server" p $ unwords
  [ show peer
  , either show show uri
  , show code
  , maybe "-" show meta ]

-- | Shorthand for @Response 20 "text/gemini"@
--
-- @since 0.1.0.0
okGemini :: LBS.ByteString -> Response
okGemini = Response 20 $ fromString "text/gemini"

-- | Shorthand for @Response 20 "text/plain"@
--
-- @since 0.1.0.0
okPlain :: LBS.ByteString -> Response
okPlain = Response 20 $ fromString "text/plain"

-- | Shorthand for @Response 30 uri@
--
-- @since 0.1.0.0
redirect :: URI -> Response
redirect uri = Response 30 (uriToString id uri "") mempty
