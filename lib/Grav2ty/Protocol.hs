{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Grav2ty.Protocol
  ( -- * Packets
    Packet (..)
  , ToPacket (..)
  , renderPacket
  , packetParser
    -- * Protocol
  , protocolVersion
  , Message (..)
  , ErrorType (..)
  , renderMessage
  , messageParser
    -- * Mappings between 'Message's and 'Grav2tyUpdate's
  , messageUpdateClient
  , messageUpdateServer
  , updateMessageServer
  ) where

import Prelude hiding (take)

import Grav2ty.Core
import qualified Grav2ty.Control as GC (Grav2tyUpdate (..))
import Grav2ty.Util.Serialization

import Control.Lens ((%=), (.=), use)
import Control.Monad (when)
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString (..))
import qualified Data.ByteString as BS
import Data.Char
import Flat
import Flat.Instances
import Data.Int
import qualified Data.Map as M
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
  | AssignMods [Modifier]
  | NewWorld Tick (World a)
  | NewObject Tick Id (Object a)
  | UpdateMod Tick Modifier (Modification a)
  | TimePerTick Int
  | DeleteObject Tick Id
-- TODO readd RequestMods Int for spectator support plus multiple users connections
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
  toPacket (AssignMods ids) = Packet 2 . flat . map (\(Mod i) -> i) . filter doesModify $ ids
  toPacket (NewWorld tick world) = Packet 3 $ flat (tick, world)
  toPacket (NewObject tick id obj) = Packet 4 $ flat (tick, id, obj)
  toPacket (UpdateMod t m mf) = Packet 5 (flat (t, m, mf))
  toPacket (TimePerTick t) = Packet 6 (flat t)
  toPacket (DeleteObject t i) = Packet 7 $ flat (t, i)

  fromPacket (Packet 0 v) = toMaybe (BS.length v == 1) (ProtocolVersion $ BS.head v)
  fromPacket (Packet 1 e) = Error <$> rightToMaybe (unflat e)
  fromPacket (Packet 2 m) = AssignMods . map Mod <$> rightToMaybe (unflat m)
  fromPacket (Packet 3 w) =
    case unflat w of
      Left _ -> Nothing
      Right (tick, world) -> Just $ NewWorld tick world
  fromPacket (Packet 4 o) =
    case unflat o of
      Left _ -> Nothing
      Right (tick, id, obj) -> Just $ NewObject tick id obj
  fromPacket (Packet 5 m) =
    case unflat m of
      Left _ -> Nothing
      Right (t, m, mf) -> Just $ UpdateMod t m mf
  fromPacket (Packet 6 t) = TimePerTick <$> rightToMaybe (unflat t)
  fromPacket (Packet 7 m) = uncurry DeleteObject <$> rightToMaybe (unflat m)
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

packetParser :: Parser Packet
packetParser = do
  t <- anyWord8
  length <- fromIntegral . unbytes . BS.unpack <$> take 8 -- TODO get rid of unpack
  Packet t <$> take length

renderMessage :: Flat a => Message a -> ByteString
renderMessage = renderPacket . toPacket

messageParser :: Flat a => Parser (Message a)
messageParser = packetParser >>=
  maybe (fail "Packet is no valid message") pure . fromPacket

clientTickUpdate :: Tick -> Tick -> [GC.Grav2tyUpdate a]
clientTickUpdate current tick =
  if tick > current
    then [GC.SetTick tick]
    else []

messageUpdateClient :: Tick -> Message a -> [GC.Grav2tyUpdate a]
messageUpdateClient _ (NewWorld t w) = [GC.SetTick t, GC.SetWorld w]
messageUpdateClient current (NewObject t i o) =
 if t < current
   then []
   else GC.UpdateObject i o : clientTickUpdate current t
messageUpdateClient current (UpdateMod t m mf) =
  if t < current
    then []
    else GC.UpdateMod m mf : clientTickUpdate current t
messageUpdateClient _ (TimePerTick tm) = [GC.SetTimePerTick tm]
messageUpdateClient _ _ = []

messageUpdateServer :: [Modifier] -> Message a -> [GC.Grav2tyUpdate a]
-- TODO factor in tick
messageUpdateServer allowed (UpdateMod _ m mf) =
  if m `elem` allowed then [GC.UpdateMod m mf] else []
messageUpdateServer _ _ = []

updateMessageServer :: Tick -> GC.Grav2tyUpdate a -> Maybe (Message a)
updateMessageServer current (GC.DeleteObject i) = Just $ DeleteObject current i
updateMessageServer current (GC.UpdateObject i o) = Just $ NewObject current i o
updateMessageServer current (GC.UpdateMod m mf) = Just $ UpdateMod current m mf
updateMessageServer current (GC.SetWorld w) = Just $ NewWorld current w
updateMessageServer current (GC.SetTimePerTick tm) = Just $ TimePerTick tm
updateMessageServer current _ = Nothing
