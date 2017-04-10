module Main where


import           Data.ByteString.Lazy as BS
import           Data.Word8
-- import           Text.Parsec          hiding (getInput)
import           Control.Applicative
import           Text.Parsec.Prim


-- MIDI File
data Midi = Midi {
             getMidiHeader :: MidiHeader
           , getTracks     :: [MidiTrack]
           }
           deriving (Show, Eq)

-- MIDI Header
data MidiHeader = MidiHeader {
                    getMidiFormat :: MidiFormat
                  , getTracksNum  :: Int
                  , getTimeUnit   :: Int -- This is delta time
                  }
                  deriving (Show, Eq)

-- MIDI Track
data MidiTrack = MidiTrack {
                   getDeltaTimeAndEvents :: [(DeltaTime, Event)]
                 }
                 deriving (Show, Eq)

-- MIDI Format
data MidiFormat = MidiFormat0 | MidiFormat1 | MidiFormat2 deriving (Show, Eq)

-- DeltaTime
data DeltaTime  = DeletaTime Integer deriving (Show, Eq)

-- Event
data Event =  NoteOff {getChannel :: Word8, getPitch :: Word8, getVelocity :: Word8}
            | NoteOn  {getChannel :: Word8, getPitch :: Word8, getVelocity :: Word8}
            | ControlChange {first :: Word8, second :: Word8}
            | MetaEvent MetaEvent
            deriving (Show, Eq)

-- Meta Event
-- TODO [Word8] should be String
data MetaEvent =  CommentEvent [Word8]
                | CopyrightEvent [Word8]
                | TrackNameEvent [Word8]
                | InstrumentNameEvent [Word8]
                | LyricsEvent [Word8]
                | TempoEvent Int
                | MeasureEvent {getNn :: Word8, getDd :: Word8, getCc :: Word8, getBb :: Word8}
                | KeyEvent {getSharpsNum :: Int, getFlastNum :: Int, getKey :: Key}
                deriving (Show, Eq)

-- Major key or Minor Key
data Key = MajorKey | MinorKey deriving (Show, Eq)

-- data ParseError =  FormatError String
--                  | UnexpectedError String

-- Get head parser
headP :: Parsec [a] u a
headP = do
  input <- getInput
  case input of
    (x:xs) -> setInput xs >> return x
    []     -> unexpected "No head in headP"

-- Take n from the source
takeP :: Int -> Parsec [a] u [a]
takeP 0 = return []
takeP n = (:) <$> headP <*> takeP (n-1)

-- General `satisfy` inspired by `satisfy` in Parsec.Char
satisfyListHead :: (a ->  Bool) -> Parsec [a] u a
satisfyListHead p = do
  x <- headP
  if p x
    then return x
    else unexpected "Error in satisfyListHead"


-- Parser for MIDI Header
midiHeaderP :: Parsec [Word8] u MidiHeader
midiHeaderP = do
  {- [NOTICE]: The order is important -}

  -- Consume(=validate) a chunk type
  chunkTypeP
  -- Get Data Length
  dataLength <- dataLengthP
  -- Get MIDI format
  midiFormat <- midiFormatP
  -- Get the number of tracks
  tracksNum  <- tracksNumP
  -- Get time unit
  timeUnit   <- timeUnitP

  return $ MidiHeader midiFormat tracksNum timeUnit

  where
    -- Parser of header-chunk-type
    chunkTypeP :: Parsec [Word8] u ()
    chunkTypeP = do
      satisfyListHead (==0x4D)
      satisfyListHead (==0x54)
      satisfyListHead (==0x68)
      satisfyListHead (==0x64)
      return ()

    -- Parser of data length
    dataLengthP :: Parsec [Word8] u Int
    dataLengthP = do
      dataLengthBytes <- takeP 4
      return 6 -- TODO Change but mostly 6

    -- Parser of MIDI Format
    midiFormatP :: Parsec [Word8] u MidiFormat
    midiFormatP = do
      formatBytes <- takeP 2
      return MidiFormat2 -- TODO implement

    -- Parser of the number of tracks
    tracksNumP :: Parsec [Word8] u Int
    tracksNumP = do
      tracksNumBytes <- takeP 2
      return (-1) -- TODO implement

    -- Parser of Time Unit
    timeUnitP :: Parsec [Word8] u Int
    timeUnitP = do
      timeUnitBytes <- takeP 2
      return (-1) -- TODO implement


main :: IO ()
main = do
  bytes <- unpack <$> BS.readFile "./yokoso.mid"
  parseTest midiHeaderP bytes
  return ()
