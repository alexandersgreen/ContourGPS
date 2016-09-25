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

-- | The specific Diagram type we shall be using
type Diagram = QDiagram Rasterific V2 Float Any

-- | Save the diagram to the given file path, with the given resolution
saveImage :: FilePath -> Int -> Int -> Diagram -> IO ()
saveImage fp width height diagram = writePng fp $ renderDia Rasterific options diagram
 where
  options = RasterificOptions $ dims2D (fromIntegral width) (fromIntegral height)

-- | Save the diagrams with the given resolution, to a series of images whose filenames are
--   based on the filepath given. Where each diagram is given a file name in numerical order
--   with the number padded so that all filenames are of the same pattern.
saveImages :: FilePath -> Int -> Int -> [Diagram] -> IO Int
saveImages fp x y ds = saveImages' 0 (length $ show $ length ds) ds >> return (length ds)
 where 
  saveImages' _ _ [] = return ()
  saveImages' n pad (d:ds) = saveImage (fp ++ (padWith pad n) ++ ".png") x y d >> saveImages' (n+1) pad ds   

-- | Given a width for padding, and an Int, return a String of that Int padded to the given width
padWith :: Int -> Int -> String
padWith pad n = '-':(replicate m '0' ++ sn)
 where
  sn = show n
  m = pad - (length sn)

-- | Given a list of loactions, generate a Diagram representing the path followed by those locations (in red).
createDiagram :: [Maybe Location] -> Diagram
createDiagram ls = fromVertices (map toVertex [l | (Just l) <- ls]) # lc red

-- | Given a list of loctions, and an underlying Diagram, generate a Diagram for each location, where the current
--   location is added as yellow point at that location.
createDiagrams :: [Maybe Location] -> Diagram -> [Diagram]
createDiagrams ls base = map (addPoint base size) ls
 where
  size = (maxlat - minlat) / 50.0
  (minlat,maxlat) = minMaxLat [l | (Just l) <- ls] 

-- | Given a list of locations, find the minimum and maximum latitude
minMaxLat :: [Location] -> (Float,Float)
minMaxLat = minMaxLat' (90.0,-90.0)
 where
  minMaxLat' m [] = m
  minMaxLat' (min,max) ((Location _ lat _):ls) = minMaxLat' (if lat < min then lat else min,if lat > max then lat else max) ls

-- | Add a yellow point to a Diagram of the given size and at the given location
addPoint :: Diagram -> Float -> (Maybe Location) -> Diagram
addPoint d size (Just l) = place (circle size # fc yellow # lc yellow) (toVertex l) `atop` d
addPoint d _ _ = d

-- | Convert a location into a 2-dimensional point.
toVertex :: Location -> Point V2 Float
toVertex (Location _ lat lon) = p2 (lon,lat)

-- | Test saving an image generated from the points in the test file
testSave :: IO ()
testSave = do
  locs <- test
  saveImage "MYTEST.png" 1024 768 (createDiagram locs)

-- | Given a Filepath to a video, generate an image of the path followed by the GPS data in that video.
createOverlay :: FilePath -> FilePath -> IO ()
createOverlay video png = do
 (locs,((x,y),_)) <- getLocations video
 saveImage png x y (createDiagram locs)

-- | Given a Filepath to a video, generate a series of images of the path followed by the GPS data in that video,
--   with a yellow point at the current location. Then, using these images, and the underlying video, generate a
--   a new video with the images forming an overlay. Note that the first Int argument needs to match the rate at
--   which the ContourGPS has been set to save GPS data. 
createOverlays :: Int -> FilePath -> FilePath -> FilePath -> IO ()
createOverlays n video png outputFile = do
 (locs,((x,y),frameRate)) <- getLocations video
 num <- saveImages png x y (createDiagrams locs (createDiagram locs))
 putStrLn $ "Saved " ++ (show num) ++ " image(s)."
 let pngBase = getBase png (length $ show num)
 saveOverlays video frameRate pngBase n outputFile
 

