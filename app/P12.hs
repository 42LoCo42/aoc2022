module P12 where

import           Data.Char   (ord)
import qualified Data.Vector as V

import Util

type Heightmap = V.Vector (V.Vector Int)

parseHeightmap :: String -> Heightmap
parseHeightmap =
  lines
  & map (
    map (
      ord
      & subtract (ord 'a')
    )
    & V.fromList
  )
  & V.fromList

startNum :: Int
startNum = ord 'S' - ord 'a'

endNum :: Int
endNum = ord 'E' - ord 'a'

startPos :: Heightmap -> (Int, Int)
startPos = undefined

