{-# LANGUAGE OverloadedStrings #-}

import Data.Map (fromList, findWithDefault, Map, insert)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Network.Socket
import Prelude hiding (lookup, take)
import Data.ByteString.Char8 (ByteString, pack)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, BufferMode(..), IOMode(ReadWriteMode))
import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import qualified Data.ByteString as S

version :: ByteString
version = "0.1.0"

{- type definitions
 - Key. Value, DB
 -}
type Key   = ByteString
type Value = ByteString
type DB    = Map Key Value

{- Command definitions
 - Get with one param, 
 - Set with two params, 
 - Ping with no params
 - Anything else maps to Unknown
 -}
data Command = Get Key
             | Set Key Value
             | Ping
             | Unknown
             deriving (Eq, Show)

{- Bulk and MultiBulk for reply handling
 -}
data Reply = Bulk (Maybe ByteString)
           | MultiBulk (Maybe [Reply])
           deriving (Eq, Show)

parseReply :: Reply -> Maybe Command
parseReply (MultiBulk (Just xs)) =
  case xs of
    [Bulk (Just "get"), Bulk (Just a)]                -> Just $ Get a
    [Bulk (Just "set"), Bulk (Just a), Bulk (Just b)] -> Just $ Set a b
    [Bulk (Just "ping")]                              -> Just $ Ping
    _                                                 -> Just Unknown
parseReply _ = Nothing

replyParser :: Parser Reply
replyParser = choice [bulk, multiBulk]

bulk :: Parser Reply
bulk = Bulk <$> do
    len <- char '$' *> signed decimal <* endOfLine
    if len < 0
        then return Nothing
        else Just <$> take len <* endOfLine

multiBulk :: Parser Reply
multiBulk = MultiBulk <$> do
    len <- char '*' *> signed decimal <* endOfLine
    if len < 0
        then return Nothing
        else Just <$> count len replyParser

hGetReplies :: Handle -> Parser a -> IO a
hGetReplies h parser = go S.empty
  where
    go rest = do
        parseResult <- parseWith readMore parser rest
        case parseResult of
            Fail _ _ s   -> error s
            Partial{}    -> error "error: partial"
            Done _ r     -> return r

    readMore = S.hGetSome h (4*1024)

crlf :: ByteString
crlf = "\r\n"

ok :: ByteString
ok = "+OK\r\n"

pong :: ByteString
pong = "+Pong\r\n"

sockHandler :: Socket -> TVar DB -> IO ()
sockHandler sock db = do
    (conn, _) <- accept sock
    handle <- socketToHandle conn ReadWriteMode
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    _ <- forkIO $ commandProcessor handle db
    sockHandler sock db

runCommand :: Handle -> Maybe Command -> TVar DB -> IO ()
runCommand handle (Just (Get key)) db = do
    m <- atomRead db
    let value = getValue m key
    S.hPutStr handle $ S.concat ["$", valLength value, crlf, value, crlf]
        where
            valLength :: Value -> ByteString
            valLength = pack . show . S.length
runCommand handle (Just (Set key value)) db = do
    updateValue (insert key value) db
    S.hPutStr handle ok
runCommand handle (Just (Ping)) db = do
    S.hPutStr handle pong
runCommand handle (Just Unknown) _ =
  S.hPutStr handle $ S.concat ["-ERR ", "unknown command", crlf]
runCommand _ Nothing _ = return ()

commandProcessor :: Handle -> TVar DB -> IO ()
commandProcessor handle db = do
  reply <- hGetReplies handle replyParser
  let command = parseReply reply
  runCommand handle command db
  commandProcessor handle db

{- Atomic read and update
 - for DB
 -}
atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn

getValue :: DB -> Key -> Value
getValue db k = findWithDefault "null" k db

main :: IO ()
main = withSocketsDo $ do
    database <- atomically $ newTVar $ fromList [("__version__", version)]
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet 7777 0)
    listen sock 2
    putStrLn "Listening on localhost 7777"
    sockHandler sock database
