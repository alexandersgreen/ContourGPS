module CreateOverlay where

import ExtractNMEA
import Codec.Picture hiding (Image)
import qualified Codec.Picture as P (Image)
import Codec.Picture.Png hiding (Image)
import Diagrams.Backend.Rasterific
import Diagrams.Core.Compile
import Diagrams.TwoD
import Diagrams.Core.Types (QDiagram)
import Diagrams hiding (Diagram)
import Data.Monoid (Any)
import Data.Colour.Names
import System.Exit

type Diagram = QDiagram Rasterific V2 Float Any

saveImage :: FilePath -> Int -> Int -> Diagram -> IO ()
saveImage fp width height diagram = writePng fp $ renderDia Rasterific options diagram
 where
  options = RasterificOptions $ dims2D (fromIntegral width) (fromIntegral height)

saveImages :: FilePath -> Int -> Int -> [Diagram] -> IO Int
saveImages fp x y ds = saveImages' 0 (length $ show $ length ds) ds >> return (length ds)
 where 
  saveImages' _ _ [] = return ()
  saveImages' n pad (d:ds) = saveImage (fp ++ (padWith pad n) ++ ".png") x y d >> saveImages' (n+1) pad ds   

padWith :: Int -> Int -> String
padWith pad n = '-':(replicate m '0' ++ sn)
 where
  sn = show n
  m = pad - (length sn)

createDiagram :: [Maybe Location] -> Diagram
createDiagram ls = fromVertices (map toVertex [l | (Just l) <- ls]) # lc red

createDiagrams :: [Maybe Location] -> Diagram -> [Diagram]
createDiagrams ls base = map (addPoint base size) ls
 where
  size = (maxlat - minlat) / 50.0
  (minlat,maxlat) = minMaxLat [l | (Just l) <- ls] 

minMaxLat :: [Location] -> (Float,Float)
minMaxLat = minMaxLat' (90.0,-90.0)
 where
  minMaxLat' m [] = m
  minMaxLat' (min,max) ((Location _ lat _):ls) = minMaxLat' (if lat < min then lat else min,if lat > max then lat else max) ls

addPoint :: Diagram -> Float -> (Maybe Location) -> Diagram
addPoint d size (Just l) = place (circle size # fc yellow # lc yellow) (toVertex l) `atop` d
addPoint d _ _ = d

toVertex :: Location -> Point V2 Float
toVertex (Location _ lat lon) = p2 (lon,lat)

testSave :: IO ()
testSave = do
  locs <- test
  saveImage "MYTEST.png" 1024 768 (createDiagram locs)

createOverlay :: FilePath -> FilePath -> IO ()
createOverlay video png = do
 (locs,((x,y),_)) <- getLocations video
 saveImage png x y (createDiagram locs)

createOverlays :: Int -> FilePath -> FilePath -> FilePath -> IO ()
createOverlays n video png outputFile = do
 (locs,((x,y),frameRate)) <- getLocations video
 num <- saveImages png x y (createDiagrams locs (createDiagram locs))
 putStrLn $ "Saved " ++ (show num) ++ " image(s)."
 let pngBase = getBase png (length $ show num)
 saveOverlays video frameRate pngBase n outputFile
 

