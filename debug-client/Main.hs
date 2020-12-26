module Main where

import Control.Monad (forever)
import Data.Attoparsec.ByteString (parseOnly)
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Posix.Signals

import Grav2ty.Protocol

grav2tyConnect :: String -> String -> IO ()
grav2tyConnect host port = do
  let hints = defaultHints { addrFlags = [AI_NUMERICSERV], addrSocketType = Datagram }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  print addr
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

  installHandler sigTERM (Catch (close sock)) Nothing
  installHandler sigINT  (Catch (close sock)) Nothing

  connect sock (addrAddress addr)

  sendAll sock . renderMessage $ (ProtocolVersion protocolVersion :: Message Double)

  forever $ do
    bytes <- recv sock (1024^2 * 100)

    case parseOnly messageParser bytes of
      Left e -> putStrLn $ "Parse error: " ++ e
      Right m -> print (m :: Message Double)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [ host, port ] -> grav2tyConnect host port
    _ -> error "wrong usage"
