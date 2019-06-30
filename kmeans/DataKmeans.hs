module DataKmeans where
import Data.List
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Control.DeepSeq


data Point = Point !Double !Double
    deriving (Show,Read,Eq)

instance NFData Point where
  rnf (Point x y) = () 

zeroPoint :: Point
zeroPoint = Point 0 0
--distância euclidiana
sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1-x2)^2) + ((y1-y2)^2)

instance Binary Point where
  put (Point a b) = put a >> put b
  get = do a <- get; b <- get; return (Point a b)

readPoints :: FilePath -> IO [Point]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Point (read (B.unpack sx)) (read (B.unpack sy))
               | (sx:sy:_) <- ls ]
  --
  return points
data Cluster
  = Cluster { k    ::  !Int
            , clCent  ::  !Point
            }
  deriving (Show,Read,Eq)

instance NFData Cluster where
  rnf (Cluster id cent) = () 
--conjuto de Cluster
makeCluster :: Int -> [Point] -> Cluster
makeCluster k points =
  Cluster { k    = k
          , clCent  = Point (a / fromIntegral count) (b / fromIntegral count)
          }
 where
  pointsum@(Point a b) = foldl' addPoint zeroPoint points
  count = length points

  addPoint :: Point -> Point -> Point
  addPoint (Point a b) (Point c d) = Point (a+c) (b+d)
