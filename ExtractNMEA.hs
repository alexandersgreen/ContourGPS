
module ExtractNMEA where

import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad (join)
import Data.Maybe
import System.IO
import System.Process
import System.Exit
import Data.List hiding (sepBy)
import Data.List.Split hiding (sepBy,endBy,oneOf)
import Text.Parsec hiding (char,string,noneOf,oneOf)
import qualified Text.Parsec as P (char,noneOf,oneOf)
import Data.Bits
import Data.Char
import Numeric
import Text.Read (readMaybe)
import Data.Time.Clock
import Data.Time.LocalTime

-- | Run the given command line, returning the exit code, a String containing standard output and
--   a String containing any error output.
readShell :: String -> IO (ExitCode,String,String)
readShell cmd = do
 putStrLn $ "Running: " ++ cmd
 (_,Just outh,Just errh,pid) <- createProcess (shell cmd){std_out = CreatePipe,std_err = CreatePipe}
 outMVar <- newEmptyMVar
 out  <- hGetContents outh
 _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()
 err  <- hGetContents errh
 _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()
 takeMVar outMVar
 takeMVar outMVar
 hClose outh
 hClose errh
 ex <- waitForProcess pid
 return (ex, out, err)

-- | Invoke ffmpeg to extract the subtitle stream from the given file, using tr to remove any unprintable characters
extractSubs :: FilePath -> IO (ExitCode,String,String)
extractSubs file = readShell $ "./ffmpeg -i \"" ++ file ++ "\" -vn -an -scodec copy -f rawvideo - | tr -dc '[:print:]\n'"

-- | append the given width into the filepath
getBase :: FilePath -> Int -> FilePath
getBase f w = f ++ "-%" ++ (show w) ++ "d.png"

-- | Invoke ffmpeg to generate a new video from the given video filepath, with the given framerate,
--   overlaying the image files at the given base filepath with the given framerate, outputting to
--   the given output file path
saveOverlays :: FilePath -> Float -> FilePath -> Int -> FilePath -> IO ()
saveOverlays video videoFrameRate overlayBase overlayFrameRate outputFile = do
 let cmd = "./ffmpeg -y -i '" ++ video ++ "' -r " ++ (show overlayFrameRate) ++ " -i '" ++ overlayBase ++ "' -filter_complex '[0:v][1:v] overlay=0:0:shortest=1 [out]' -map \"[out]\" -vcodec libx264 -r " ++ (show videoFrameRate) ++ " " ++ outputFile
 putStrLn $ "Running: " ++ cmd
 callCommand cmd

-- | Get a String of the video info from the output of ffmpeg
getVideoInfo :: String -> String
getVideoInfo info = unlines [x | Just x <- map isVideoInfo (lines info)]
 where
  isVideoInfo s = case isInfixOf "Video" s of
   True -> Just s
   False -> Nothing

-- | a type to represent a video resolution
type Resolution = (Int,Int)
-- | a type to represent frame rates
type FramesPerSecond = Float
-- | a type to represent the resolution and frame rate of a video
type VideoMetaData = (Resolution,FramesPerSecond)

-- | Extract the resolution and frame rate from the video info string output by ffmpeg
videoInfo :: String -> VideoMetaData
videoInfo info = ((x,y),read fps)
 where 
  [_,_,a,_,b] = take 5 $ splitOn "," $ getVideoInfo info
  [_,xy] = take 2 $ splitOn " " a
  [x,y] = map read $ take 2 $ splitOn "x" xy
  [_,fps] = take 2 $ splitOn " " b

-- | Extract the subtitles and the video meta data from the given video
extractNMEA :: FilePath -> IO (String,VideoMetaData)
extractNMEA file = do
 (_,subs,info) <- extractSubs file
 putStrLn info
 return (subs, videoInfo info) 

-- | The Parsec Parser we define will contain an Int state which represents
--   a checksum of the previously parsed characters
type Parser a = Parsec String Int a

-- | parse the given char, and modify the checksum state
char :: Char -> Parser Char
char c = do
 c' <- P.char c
 modifyState (xor (ord c'))
 return c'

-- | parse the given string
string :: String -> Parser String
string [] = return ""
string (x:xs) = do
 x' <- char x
 xs' <- string xs
 return (x':xs')

-- | noneOf and update checksum
noneOf :: String -> Parser Char
noneOf s = do
 c' <- P.noneOf s
 modifyState (xor (ord c'))
 return c'

-- | oneOf and update checksum
oneOf :: String -> Parser Char
oneOf s = do
 c' <- P.oneOf s
 modifyState (xor (ord c'))
 return c'

data EorW' = E | W deriving (Show,Eq)
type EorW = Maybe EorW'

data M' = M deriving (Show,Eq)
type M = Maybe M'

data AorV' = A | V deriving (Show,Eq)
type AorV = Maybe AorV'

type Latitude' = Float
type Longitude' = Float
type Quality' = Int
type NumberOfSatellites' = Int
type HDOP' = Float
type Altitude' = Float
type GeoidalSeparation' = Float
type AgeDiff' = Float
type DiffId' = Int
type Speed' = Float
type DegreesTrue' = Float
type Date' = Int
type Variation' = Float

type Timestamp = Maybe DiffTime
type Latitude = Maybe Latitude'
type Longitude = Maybe Longitude'
type Quality = Maybe Quality'
type NumberOfSatellites = Maybe NumberOfSatellites'
type HDOP = Maybe HDOP'
type Altitude = Maybe Altitude'
type GeoidalSeparation = Maybe GeoidalSeparation'
type AgeDiff = Maybe AgeDiff'
type DiffId = Maybe DiffId'
type Speed = Maybe Speed'
type DegreesTrue = Maybe DegreesTrue'
type Date = Maybe Date'
type Variation = Maybe Variation'

data NMEA =
   GPGGA Timestamp Latitude Longitude Quality NumberOfSatellites HDOP Altitude M GeoidalSeparation M AgeDiff DiffId
 | GPRMC Timestamp AorV Latitude Longitude Speed DegreesTrue Date Variation EorW
 deriving Show

-- | Parse multiple NMEA sentences
nmea :: Parser [Maybe (NMEA,Bool)]
nmea = do
 sentences <- many (nmeaSentence >>= \x -> char '\n' >> return x)
 eof
 return sentences

-- | Parse an NMEA sentence, checking the checksum
nmeaSentence :: Parser (Maybe (NMEA,Bool))
nmeaSentence = do
 mnmea <- optionMaybe $ try nmeaSentence'
 many $ noneOf "\n"
 case mnmea of
  Nothing -> return Nothing
  (Just (s,cs,cs')) -> case cs' of
                        Just cs' -> return $ Just (s, cs == cs')
                        Nothing -> return $ Just (s, False)

-- | Parse an NMEA sentece, including the generated checksum, and the parsed checksum
nmeaSentence' :: Parser (NMEA, Int, Maybe Int)
nmeaSentence' = do
 many $ noneOf "$"
 char '$'
 (s,cs) <- withChecksum sentences
 char '*'
 cs' <- optionMaybeTo '\n' intFromHex
 return (s,cs,join cs')

-- | A wrapper function that resets the checksum before parsing, and then returns
--   the generated checksum along with the result of parsing
withChecksum :: Parser a -> Parser (a,Int)
withChecksum p = do
 putState 0
 s <- p
 cs <- getState
 return (s,cs)

-- | Parse an NMEA sentence, which is either a GPGGA or GPRMC sentence
sentences :: Parser NMEA
sentences = choice [gpgga,gprmc]

-- | Parse a GPGGA sentence
gpgga :: Parser NMEA
gpgga = do
 try $ string "GPGGA,"
 timestamp <- optionMaybe timestamp
 char ','
 latitude <- optionMaybe latitude
 char ','
 longitude <- optionMaybe longitude
 char ',' 
 quality <- optionNatTo ','
 char ','
 numberOfSatellites <- optionNatTo ','
 char ','
 hdop <- optionFloatTo ','
 char ','
 altitude <- optionFloatTo ','
 char ','
 m1 <- optionMaybeTo ',' m
 char ','
 geoidalSeparation <- optionFloatTo ','
 char ','
 m2 <- optionMaybeTo ',' m
 char ','
 ageDiff <- optionFloatTo ','
 char ','
 diffId <- optionNatTo '*'
 return $ GPGGA timestamp latitude longitude quality numberOfSatellites hdop altitude m1 geoidalSeparation m2 ageDiff diffId

-- | Parse a GPRMC sentence
gprmc :: Parser NMEA
gprmc = do
 try $ string "GPRMC,"
 timestamp <- optionMaybe timestamp
 char ','
 av <- optionMaybeTo ',' aOrV
 char ','
 latitude <- optionMaybe latitude
 char ','
 longitude <- optionMaybe longitude
 char ','
 speed <- optionFloatTo ','
 char ','
 degreesTrue <- optionFloatTo ','
 char ','
 date <- optionNatTo ','
 char ','
 variation <- optionFloatTo ','
 char ','
 ew <- optionMaybeTo '*' eOrW
 return $ GPRMC timestamp av latitude longitude speed degreesTrue date variation ew

-- | Try the given parser, and consume input after that until the given character
optionMaybeTo :: Char -> Parser a -> Parser (Maybe a)
optionMaybeTo c p = do
 r <- optionMaybe p
 many $ noneOf [c]
 return r

-- | Parse a Natural Number as an Int
nat :: Parser Int
nat = do
 s <- many1 $ oneOf "0123456789"
 return $ read s

optionNatTo c = optionMaybeTo c nat 

-- | Parse a Natural Number using only the next n chars.
natFromNext :: Int -> Parser Int
natFromNext n = do
 s <- count n $ oneOf "0123456789"
 return $ read s      

-- | Parse until the given char, and read as a float
floatTo :: Char -> Parser Float
floatTo c = do
 s <- many $ noneOf [c]
 return $ read s

optionFloatTo c = optionMaybe (floatTo c) 

-- | Parse a timestamp formatted as hhmmss.ss*,
timestamp :: Parser DiffTime
timestamp = do
 h <- natFromNext 2
 m <- natFromNext 2
 s <- floatTo ','
 return $ toDiffTime h m s

-- | Create a DiffTime from the given hours, mintues, and seconds.
toDiffTime :: Int -> Int -> Float -> DiffTime
toDiffTime h m s = picosecondsToDiffTime pico
 where
  pico = (toInteger milli) * 1000000000
  milli = (secs * 1000) + (round (s * 1000))
  secs = mins * 60
  mins = (h * 60) + m  

-- | Parse a latitude
latitude :: Parser Latitude'
latitude = do
 degs <- natFromNext 2
 mins <- floatTo ','
 let lat = decimalDegrees degs mins
 char ','
 nOrS <- choice [try (char 'N'),char 'S'] 
 return $ if (nOrS == 'N') then lat else -lat

-- | Parse a longitude
longitude :: Parser Longitude'
longitude = do
 degs <- natFromNext 3
 mins <- floatTo ','
 let lon = decimalDegrees degs mins
 char ','
 eOrW <- choice [try (char 'E'),char 'W'] 
 return $ if (eOrW == 'E') then lon else -lon

-- | Convert an angle from Degrees\Minutes into a decimal angle
decimalDegrees :: Int -> Float -> Float
decimalDegrees degs mins = (fromIntegral degs) + (mins / 60)

-- | Parse and E or a W
eOrW :: Parser EorW'
eOrW = choice [e,w]
 where
  e = try (char 'E') >> return E
  w = try (char 'W') >> return W

-- | Parse an M
m :: Parser M'
m = char 'M' >> return M

-- | Parse and A or a V
aOrV :: Parser  AorV'
aOrV = choice [a,v]
 where
  a = try (char 'A') >> return A
  v = try (char 'V') >> return V

-- | Parse a HexString and return as an Int if possible
intFromHex :: Parser (Maybe Int)
intFromHex = do
 s <- many (noneOf "\n")          
 return $ optionReadHex s

optionReadHex :: String -> Maybe Int
optionReadHex s = case readHex s of
 [(cs,_)] -> Just cs
 _ -> Nothing

-- | Given a video file, extract the video info and NMEA data,
--   parse the NMEA data, and return the result along with the video info and the String
--   that was parsed
parseFile' :: FilePath -> IO (Either ParseError [Maybe (NMEA,Bool)],VideoMetaData,String)
parseFile' f = do
 (s,info) <- extractNMEA f
 return $ (runParser nmea 0 f s, info, s)

-- | Given a video file, extract the video info and NEMA data,
--   parse each line of NMEA data separately, returning the results and the video info
parseFile :: FilePath -> IO ([Either ParseError (Maybe (NMEA,Bool))],VideoMetaData)
parseFile f = do
 (s,info) <- extractNMEA f
 return $ (map (runParser nmeaSentence 0 f) (lines s), info)

-- | A location contains a timestamp, as well as latitude and longitude
data Location = Location {
     time :: DiffTime,
     lat :: Float,
     lon :: Float
 } deriving (Eq,Ord)

instance Show Location where
 show (Location time lat lon) = "(" ++ show (timeToTimeOfDay time) ++ "," ++ show lat ++ "," ++ show lon ++ ")"

-- | Any type that can possibly return a location, can be thought of as a LocationProvider
class LocationProvider a where
 getLocation :: a -> Maybe Location

-- | NMEA data can be used as a Location provider.
--   Note we currently only use the GPGGA sentences, as the ContourGPS creates a GPGGA and GPRMC for the same
--   time stamp, and that means we would be duplcating each location. We would need both sentences if\when we
--   want to extract more information than just locations...
instance LocationProvider NMEA where
 getLocation (GPGGA (Just time) (Just lat) (Just lon) _ _ _ _ _ _ _ _ _) = Just $ Location time lat lon
 --getLocation (GPRMC (Just time) _ (Just lat) (Just lon) _ _ _ _ _) = Just $ Location time lat lon
 getLocation _ = Nothing

-- | Given a video file, try to extract the locations and the video info, failing to parse if any line of
-- NMEA data fails.
getLocations' :: FilePath -> IO ([Maybe Location],VideoMetaData)
getLocations' file = do
 (exs,vmd,s) <- parseFile' file
 case exs of
  (Right xs) -> return ([ getLocation x | Just (x,True) <- xs], vmd)
  (Left e) -> do
    printErrorLine e s
    error $ show e

-- | Given a video file, try to extract the locations and the video info, parsing each line of NMEA data
-- separately
getLocations :: FilePath -> IO ([Maybe Location],VideoMetaData)
getLocations file = do
 (exs,vmd) <- parseFile file
 return ([ getLocation n | Right (Just (n@(GPGGA _ _ _ _ _ _ _ _ _ _ _ _),True)) <- exs], vmd)
 
-- | A helper function to only print a few lines from the source String, surrouding a ParseError    
printErrorLine :: ParseError -> String -> IO ()
printErrorLine e s = printLines (getLinesUpto 4 (sourceLine (errorPos e)) (lines s))

-- | Print the given Strings, one per line
printLines :: [String] -> IO ()
printLines [] = return ()
printLines (x:xs) = putStrLn x >> printLines xs

getLinesUpto :: Int -> Int -> [String] -> [String]
getLinesUpto num l s = drop (l - num) (take l s)

-- | A file used for testing (which isn't in the repo)
testFile :: FilePath
testFile = "/alex/GPS/cycle.mov"

-- | Extract all the locations from the test file
test :: IO [Maybe Location]
test = getLocations testFile >>= (return . fst)

