module Miner where

import Utility (toHex, toWord8, toWord32, toStr)
import Data.Word (Word8, Word32)
import Data.Digest.SHA2 (toOctets, sha256)
import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)
import Network.HTTP (Request(..), RequestMethod(..), HeaderName(..), mkHeader, getAuth, host, port, sendHTTP, getResponseBody)
import Network.TCP (HandleStream, openStream, HStream, close)
import qualified Codec.Binary.Base64.String as Base64 (encode)
import qualified Text.JSON as JSON (JSObject, JSValue, Result(..), decode, valFromObj)

-- A prefix consists of the all block headers except nonce:
-- Version, Previous Hash, Merkle Root, Timestamp, Target.
data Question = Question {prefix :: [Word8], nonce :: Word32, target :: [Word8]}

-- @getQuestion body@ gets a question from a section of JSON text, being retrieved from a mining pool via jsonrpc.
getQuestion :: String -> JSON.Result Question
getQuestion body = do
    result <- responseResult body
    d <- JSON.valFromObj "data" result
    t <- JSON.valFromObj "target" result
    let (p, xs) = splitAt 152 d
    let n = take 8 xs
    return $ Question (toHex p) (toWord32 . toHex $ n) (reverse . toHex $ t)

-- @responseResult body@ gets the value of key "result" from a section of JSON text, being retrieved from a mining pool via jsonrpc.
responseResult :: String -> JSON.Result (JSON.JSObject JSON.JSValue)
responseResult body = do
    mainObject <- JSON.decode body
    JSON.valFromObj "result" mainObject

type Answer = [Word8]

-- @answer question@ get the first answer from the candidates, which are less than the target, and it should be little-endian.
answer :: Question -> Answer
answer (Question p n t) = reverse . head $ answers where
    answers = filter (< t) . fmap hashBlockHeader $ headers
    headers = fmap (makeHeaderWithPrefix p) nonces
    nonces = nonceRange n

-- Core algorithm: sha256(sha256(BlockHeader))
hashBlockHeader :: [Word8] -> [Word8]
hashBlockHeader = toOctets . sha256 . toOctets . sha256

nonceRange :: Word32 -> [Word32]
nonceRange start = [start..maxBound]

makeHeaderWithPrefix :: [Word8] -> Word32 -> [Word8]
makeHeaderWithPrefix prefix nonce = prefix ++ toWord8 nonce

-- @getWorkRequest uri auth@ make a getwork HTTP request which can be send to the mining pool. @auth@ is a basic authorization string.
getWorkRequest :: URI -> String -> Request String
getWorkRequest = makeRequestWithBody "{\"id\":\"jsonrpc\",\"method\":\"getWork\",\"params\":[]}"

-- @putBackAnswerRequest uri auth answer@ make a HTTP request to send back the answer.
putBackAnswerRequest :: Answer -> URI -> String -> Request String
putBackAnswerRequest answer = makeRequestWithBody body where
    body = "{\"id\":\"jsonrpc\",\"method\":\"getWork\",\"params\":[" ++ show (toStr answer) ++ "]}"

makeRequestWithBody :: String -> URI -> String -> Request String
makeRequestWithBody body uri auth = Request {
    rqURI = uri,
    rqMethod = POST,
    rqHeaders = [
        mkHeader HdrAuthorization auth,
        mkHeader HdrContentType "application/json",
        mkHeader HdrContentLength (show . length $ body)
    ],
    rqBody = body
}

-- @basicAuthString user password@ create a basic authorization string with your user and password.
basicAuthString :: String -> String -> String
basicAuthString user password = "Basic " ++ credentials where
    credentials = Base64.encode $ user ++ ":" ++ password

-- @openStreamWithRequest r@ open a HandleStream with a HTTP request.
openStreamWithRequest :: (HStream bufType) => Request a -> IO (HandleStream bufType)
openStreamWithRequest r = do
    auth <- getAuth r
    openStream (host auth) (fromMaybe 80 (port auth))

mine :: String -> String -> String -> IO ()
mine uriString user password = do
    let (Just uri) = parseURI uriString
    let auth = basicAuthString user password
    let req = getWorkRequest uri auth
    hStream <- openStreamWithRequest req
    body <- (sendHTTP hStream req >>= getResponseBody)
    let (JSON.Ok question) = getQuestion body
    let answerReq = putBackAnswerRequest (answer question) uri auth
    body <- (sendHTTP hStream answerReq >>= getResponseBody)
    close hStream
    print body
