import DataKmeans
import Data.Random.Normal
import System.Random
import System.IO
import Data.Array
import System.Environment
import Control.Monad
import Data.List
import Data.Binary

minX, maxX, minY, maxY, minSD, maxSD :: Double
minX = -10
maxX = 10
minY = -10
maxY = 10
minSD = 1.5
maxSD = 2.0

main :: IO ()
main = do
    n: minp: maxp: rest <- fmap (fmap read) getArgs

    case rest of
        [seed] -> setStdGen (mkStdGen seed)
        _ -> return ()

    nps <- replicateM n (randomRIO (minp, maxp))
    xs  <- replicateM n (randomRIO (minX, maxX))
    ys  <- replicateM n (randomRIO (minY, maxY))
    sds <- replicateM n (randomRIO (minSD, maxSD))

    let params = zip5 nps xs ys sds sds

    
    ss <- mapM (\(a,b,c,d,e) -> generate2DSamples a b c d e) params
    let points = concat ss

    
    hsamp <- openFile "points" WriteMode
    mapM_ (printPoint hsamp) points
    hClose hsamp

    encodeFile "points.bin" points

    -- gerar os clusters inicias de cada ponto
    -- cluster.
    gen <- newStdGen
    let
        rand_clusters = randomRs (0,n-1) gen :: [Int]
        arr = accumArray (flip (:)) [] (0,n-1) $
                zip rand_clusters points
        clusters = map (uncurry makeCluster) (assocs arr)
    writeFile "clusters" (show clusters)

    
    writeFile "params" (show params)


printPoint :: Handle -> Point -> IO ()
printPoint h (Point x y) = do
  hPutStr h (show x)
  hPutChar h ' '
  hPutStr h (show y)
  hPutChar h '\n'

generate2DSamples :: Int                 -- numero gerador 
                  -> Double -> Double    -- X e Y da média
                  -> Double -> Double    -- Desvios padrão X e Y
                  -> IO [Point]
generate2DSamples n mx my sdx sdy = do
  gen <- newStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return (zipWith Point (take n xsamples) ysamples)
