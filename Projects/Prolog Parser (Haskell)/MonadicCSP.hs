module MonadicCSP where

import Data.List (delete)

data Antenna   = UAT     | LAT   deriving (Eq, Show, Enum)
data Hexplexer = TX_MB   | TX_HB deriving (Eq, Show, Enum)
data Band      = B3_4_TX | B1_TX  | B1_4_RX | B3_RX | B7_TX  | B7_RX  |
                 B4_TX   | B25_TX | B25_RX  | B4_RX | B30_RX | B30_TX
               deriving (Eq, Show, Enum)

allAntennas :: [Antenna]
allAntennas = enumFrom (toEnum 0 :: Antenna)

allBands :: [Band]
allBands = enumFrom (toEnum 0 :: Band)

txBands :: [Band]
txBands = [B3_4_TX, B1_TX, B7_TX, B4_TX, B25_TX, B30_TX]

onHexp :: Band -> Hexplexer
onHexp B3_4_TX = TX_MB; onHexp B1_TX  = TX_MB; onHexp B1_4_RX = TX_MB
onHexp B3_RX   = TX_MB; onHexp B7_TX  = TX_MB; onHexp B7_RX   = TX_MB;
onHexp B4_TX   = TX_HB; onHexp B25_TX = TX_HB; onHexp B25_RX  = TX_HB;
onHexp B4_RX   = TX_HB; onHexp B30_RX = TX_HB; onHexp B30_TX  = TX_HB;

meTXBands :: [[Band]]
meTXBands = [[B3_4_TX, B1_TX, B4_TX, B25_TX], [B7_TX, B30_TX]]

elems :: Eq a => a -> a -> [a] -> Bool
elems e1 e2 l = e1 `elem` l && e2 `elem` l

mutuallyExclusive :: Band -> Band -> Bool
mutuallyExclusive b1 b2 = elems b1 b2 txBands && b1 /= b2 && meTXBandsCheck
  where meTXBandsCheck = or $ meTXBands >>= \l -> return $ elems b1 b2 l

cABAB :: Antenna -> Band -> Antenna -> Band -> Bool
cABAB a1 b1 a2 b2 = b1 /= b2 && not (mutuallyExclusive b1 b2) &&
                    ((a1 /= a2 && onHexp b1 /= onHexp b2) ||
                     (a1 == a2 && onHexp b1 == onHexp b2))

cAN :: Antenna -> [Band] -> Bool
cAN a l = and $ l >>= \e1 -> delete e1 l >>= \e2 -> return $ cABAB a e1 a e2

cABAN :: Antenna -> Band -> Antenna -> [Band] -> Bool
cABAN a1 b a2 l = and $ l >>= \e -> return $ cABAB a1 b a2 e

cANAN :: Antenna -> [Band] -> Antenna -> [Band] -> Bool
cANAN a1 l1 a2 l2 = cAN a1 l1 && cAN a2 l2 &&
                    and (l1 >>= \e1 -> return $ cABAN a1 e1 a2 l2) &&
                    and (l2 >>= \e2 -> return $ cABAN a2 e2 a1 l1)
