module P13 where

import Data.Char                    (isDigit)
import Data.Functor                 (void)
import Text.ParserCombinators.ReadP

import Util

data Packet
  = PacketNum Int
  | PacketLst [Packet]
  deriving (Eq)

instance Read Packet where
  readsPrec _ = readP_to_S readPacket
    where
      readPacket    = readPacketNum +++ readPacketLst
      readPacketNum = satisfy isDigit % many1 % fmap (read & PacketNum)
      readPacketLst = do
        void $ char '['
        items <- sepBy readPacket (char ',')
        void $ char ']'
        PacketLst items % pure

instance Show Packet where
  show (PacketNum n) = show n
  show (PacketLst l) = show l

instance Ord Packet where
  compare   (PacketNum n1)   (PacketNum n2) = compare n1 n2
  compare n@(PacketNum _ ) l@(PacketLst _ ) = compare (PacketLst [n]) l
  compare l@(PacketLst _ ) n@(PacketNum _ ) = compare n l % flipOrder
  compare   (PacketLst l1)   (PacketLst l2) =
    zipWith compare l1 l2
    % dropWhile (== EQ)
    % safeHead (compare (length l1) (length l2))
