{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Grav2ty.Protocol where

import Prelude hiding (take)

import Grav2ty.Core
import Grav2ty.Util.Serialization

import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString (..))
import qualified Data.ByteString as BS
import Data.Char
import Data.Flat
import Data.Flat.Instances
import Data.Int
import Data.Maybe (fromJust, isNothing)
import Data.Word
import Linear.V2

data Packet
  = Packet
  { pMessageType    :: Word8
  , pPacketContents :: ByteString
  } deriving (Show, Eq, Ord)

class ToPacket a where
  toPacket   :: a -> Packet
  fromPacket :: Packet -> Maybe a

protocolVersion :: Word8
protocolVersion = 1

data ErrorType
  = ErrorServerFull
  | ErrorVersionMismatch
  | ErrorNoParse
  deriving (Show, Eq, Ord, Generic, Flat)

-- | Protocol Version 1
data Message a
  = ProtocolVersion Word8
  | Error ErrorType
  | AssignMods [Id]
  | NewWorld Tick (World a)
  | NewObject Tick Id (Object a)
  | UpdateMod (ModMap a)
  | TicksPerSecond Tick
  deriving (Show, Eq, Ord, Generic, Flat)

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x

instance Flat a => ToPacket (Message a) where
  toPacket (ProtocolVersion v) = Packet 0 (BS.singleton v)
  toPacket (Error e) = Packet 1 (flat e)
  toPacket (AssignMods ids) = Packet 2 (flat ids)
  toPacket (NewWorld tick world) = Packet 3 $ flat (tick, world)
  toPacket (NewObject tick id obj) = Packet 4 $ flat (tick, id, obj)
  toPacket (UpdateMod modmap) = Packet 5 (flat modmap)
  toPacket (TicksPerSecond t) = Packet 6 (flat t)
  fromPacket (Packet 0 v) = toMaybe (BS.length v == 1) (ProtocolVersion $ BS.head v)
  fromPacket (Packet 1 e) = Error <$> rightToMaybe (unflat e)
  fromPacket (Packet 2 m) = AssignMods <$> rightToMaybe (unflat m)
  fromPacket (Packet 3 w) = case unflat w of
                              Left _ -> Nothing
                              Right (tick, world) -> Just $ NewWorld tick world
  fromPacket (Packet 4 o) = case unflat o of
                              Left _ -> Nothing
                              Right (tick, id, obj) -> Just $ NewObject tick id obj
  fromPacket (Packet 5 m) = UpdateMod <$> rightToMaybe (unflat m)
  fromPacket (Packet 6 t) = TicksPerSecond <$> rightToMaybe (unflat t)
  fromPacket (Packet _ _) = Nothing

bytes :: Int64 -> [Word8]
bytes i = bytes' 7
  where bytes' :: Int -> [Word8]
        bytes' (-1) = []
        bytes' n    = (fromIntegral . (flip shift (-8 * n)) $
            i .&. shift 0xff (8 * n))
          : bytes' (n - 1)

unbytes :: [Word8] -> Int64
unbytes l = unbytes' l 7 0
  where unbytes' [x] 0 acc    = (fromIntegral x) + acc
        unbytes' [x] n acc    = shift (fromIntegral x + acc) (-8 * n)
        unbytes' (x:xs) n acc = unbytes' xs (n - 1) (acc + shift (fromIntegral x) (8 * n))

renderPacket :: Packet -> ByteString
renderPacket (Packet t content) = BS.pack (t : bytes len) `BS.append` content
  where len :: Int64
        len = fromIntegral $ BS.length content

parsePacket :: ByteString -> Result Packet
parsePacket = parse packetParser

packetParser :: Parser Packet
packetParser = do
  t <- anyWord8
  length <- fromIntegral . unbytes . BS.unpack <$> take 8 -- TODO get rid of unpack
  Packet t <$> take length

renderMessage :: Flat a => Message a -> ByteString
renderMessage = renderPacket . toPacket

parseMessage :: Flat a => ByteString -> Result (Message a)
parseMessage = parse messageParser

messageParser :: Flat a => Parser (Message a)
messageParser = packetParser >>=
  maybe (fail "Packet is no valid message") pure . fromPacket
