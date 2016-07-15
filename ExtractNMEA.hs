
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

type Parser a = Parsec String Int a

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

extractSubs :: FilePath -> IO (ExitCode,String,String)
extractSubs file = readShell $ "./ffmpeg -i \"" ++ file ++ "\" -vn -an -scodec copy -f rawvideo - | tr -dc '[:print:]\n'"

getBase :: FilePath -> Int -> FilePath
getBase f w = f ++ "-%" ++ (show w) ++ "d.png"

saveOverlays :: FilePath -> Float -> FilePath -> Int -> FilePath -> IO ()
saveOverlays video videoFrameRate overlayBase overlayFrameRate outputFile = do
 let cmd = "./ffmpeg -y -i '" ++ video ++ "' -r " ++ (show overlayFrameRate) ++ " -i '" ++ overlayBase ++ "' -filter_complex '[0:v][1:v] overlay=0:0:shortest=1 [out]' -map \"[out]\" -vcodec libx264 -r " ++ (show videoFrameRate) ++ " " ++ outputFile
 putStrLn $ "Running: " ++ cmd
 callCommand cmd

getVideoInfo :: String -> String
getVideoInfo info = unlines [x | Just x <- map isVideoInfo (lines info)]
 where
  isVideoInfo s = case isInfixOf "Video" s of
   True -> Just s
   False -> Nothing

type Resolution = (Int,Int)
type FramesPerSecond = Float
type VideoMetaData = (Resolution,FramesPerSecond)

videoInfo :: String -> VideoMetaData
videoInfo info = ((x,y),read fps)
 where 
  [_,_,a,_,b] = take 5 $ splitOn "," $ getVideoInfo info
  [_,xy] = take 2 $ splitOn " " a
  [x,y] = map read $ take 2 $ splitOn "x" xy
  [_,fps] = take 2 $ splitOn " " b

extractNMEA :: FilePath -> IO (String,VideoMetaData)
extractNMEA file = do
 (_,subs,info) <- extractSubs file
 putStrLn info
 return (subs, videoInfo info) 

char :: Char -> Parser Char
char c = do
 c' <- P.char c
 modifyState (xor (ord c'))
 return c'

string :: String -> Parser String
string [] = return ""
string (x:xs) = do
 x' <- char x
 xs' <- string xs
 return (x':xs')

noneOf :: String -> Parser Char
noneOf s = do
 c' <- P.noneOf s
 modifyState (xor (ord c'))
 return c'

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

nmea :: Parser [Maybe (NMEA,Bool)]
nmea = do
 sentences <- many (nmeaSentence >>= \x -> char '\n' >> return x)
 eof
 return sentences

nmeaSentence :: Parser (Maybe (NMEA,Bool))
nmeaSentence = do
 mnmea <- optionMaybe $ try nmeaSentence'
 many $ noneOf "\n"
 case mnmea of
  Nothing -> return Nothing
  (Just (s,cs,cs')) -> case cs' of
                        Just cs' -> return $ Just (s, cs == cs')
                        Nothing -> return $ Just (s, False)

nmeaSentence' :: Parser (NMEA, Int, Maybe Int)
nmeaSentence' = do
 many $ noneOf "$"
 char '$'
 (s,cs) <- withChecksum sentences
 char '*'
 cs' <- optionMaybeTo '\n' intFromHex
 return (s,cs,join cs')

withChecksum :: Parser a -> Parser (a,Int)
withChecksum p = do
 putState 0
 s <- p
 cs <- getState
 return (s,cs)

sentences :: Parser NMEA
sentences = choice [gpgga,gprmc]

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

optionMaybeTo :: Char -> Parser a -> Parser (Maybe a)
optionMaybeTo c p = do
 r <- optionMaybe p
 many $ noneOf [c]
 return r

nat :: Parser Int
nat = do
 s <- many1 $ oneOf "0123456789"
 return $ read s

optionNatTo c = optionMaybeTo c nat 

natFromNext :: Int -> Parser Int
natFromNext n = do
 s <- count n $ oneOf "0123456789"
 return $ read s      

floatTo :: Char -> Parser Float
floatTo c = do
 s <- many $ noneOf [c]
 return $ read s

optionFloatTo c = optionMaybe (floatTo c) 

timestamp :: Parser DiffTime
timestamp = do
 h <- natFromNext 2
 m <- natFromNext 2
 s <- floatTo ','
 return $ toDiffTime h m s

toDiffTime :: Int -> Int -> Float -> DiffTime
toDiffTime h m s = picosecondsToDiffTime pico
 where
  pico = (toInteger milli) * 1000000000
  milli = (secs * 1000) + (round (s * 1000))
  secs = mins * 60
  mins = (h * 60) + m  


latitude :: Parser Latitude'
latitude = do
 degs <- natFromNext 2
 mins <- floatTo ','
 let lat = decimalDegrees degs mins
 char ','
 nOrS <- choice [try (char 'N'),char 'S'] 
 return $ if (nOrS == 'N') then lat else -lat

longitude :: Parser Longitude'
longitude = do
 degs <- natFromNext 3
 mins <- floatTo ','
 let lon = decimalDegrees degs mins
 char ','
 eOrW <- choice [try (char 'E'),char 'W'] 
 return $ if (eOrW == 'E') then lon else -lon

decimalDegrees :: Int -> Float -> Float
decimalDegrees degs mins = (fromIntegral degs) + (mins / 60)

eOrW :: Parser EorW'
eOrW = choice [e,w]
 where
  e = try (char 'E') >> return E
  w = try (char 'W') >> return W

m :: Parser M'
m = char 'M' >> return M

aOrV :: Parser  AorV'
aOrV = choice [a,v]
 where
  a = try (char 'A') >> return A
  v = try (char 'V') >> return V

intFromHex :: Parser (Maybe Int)
intFromHex = do
 s <- many (noneOf "\n")          
 return $ optionReadHex s

optionReadHex :: String -> Maybe Int
optionReadHex s = case readHex s of
 [(cs,_)] -> Just cs
 _ -> Nothing

parseFile' :: FilePath -> IO (Either ParseError [Maybe (NMEA,Bool)],VideoMetaData,String)
parseFile' f = do
 (s,info) <- extractNMEA f
 return $ (runParser nmea 0 f s, info, s)

parseFile :: FilePath -> IO ([Either ParseError (Maybe (NMEA,Bool))],VideoMetaData)
parseFile f = do
 (s,info) <- extractNMEA f
 return $ (map (runParser nmeaSentence 0 f) (lines s), info)

data Location = Location {
     time :: DiffTime,
     lat :: Float,
     lon :: Float
 } deriving (Eq,Ord)

instance Show Location where
 show (Location time lat lon) = "(" ++ show (timeToTimeOfDay time) ++ "," ++ show lat ++ "," ++ show lon ++ ")"

class LocationProvider a where
 getLocation :: a -> Maybe Location

instance LocationProvider NMEA where
 getLocation (GPGGA (Just time) (Just lat) (Just lon) _ _ _ _ _ _ _ _ _) = Just $ Location time lat lon
 --getLocation (GPRMC (Just time) _ (Just lat) (Just lon) _ _ _ _ _) = Just $ Location time lat lon
 getLocation _ = Nothing

getLocations' :: FilePath -> IO ([Maybe Location],VideoMetaData)
getLocations' file = do
 (exs,vmd,s) <- parseFile' file
 case exs of
  (Right xs) -> return ([ getLocation x | Just (x,True) <- xs], vmd)
  (Left e) -> do
    printErrorLine e s
    error $ show e

getLocations :: FilePath -> IO ([Maybe Location],VideoMetaData)
getLocations file = do
 (exs,vmd) <- parseFile file
 return ([ getLocation n | Right (Just (n@(GPGGA _ _ _ _ _ _ _ _ _ _ _ _),True)) <- exs], vmd)
 
    
printErrorLine :: ParseError -> String -> IO ()
printErrorLine e s = printLines (getLinesUpto 4 (sourceLine (errorPos e)) (lines s))

printLines :: [String] -> IO ()
printLines [] = return ()
printLines (x:xs) = putStrLn x >> printLines xs

getLinesUpto :: Int -> Int -> [String] -> [String]
getLinesUpto num l s = drop (l - num) (take l s)


testFile :: FilePath
testFile = "/alex/GPS/cycle.mov"

test :: IO [Maybe Location]
test = getLocations testFile >>= (return . fst)

