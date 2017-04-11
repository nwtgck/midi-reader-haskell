module Main where


import           Control.Applicative
import           Data.Bits
import           Data.ByteString.Lazy as BS
import           Data.Word8
import           Text.Parsec          hiding (getInput)
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
                   getDeltaTimesAndEvents :: [(DeltaTime, MidiEvent)]
                 }
                 deriving (Show, Eq)

-- MIDI Format
data MidiFormat = MidiFormat0 | MidiFormat1 | MidiFormat2 deriving (Show, Eq)

-- DeltaTime
data DeltaTime  = DeltaTime Integer deriving (Show, Eq)

-- Event
data MidiEvent =  NoteOff {getChannel :: Word8, getPitch :: Word8, getVelocity :: Word8}
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


-- Convert [Word8] to (Num a => a)
-- hint: http://stackoverflow.com/a/31208816/2885946
bytesToNum :: (Num a, Bits a) => [Word8] -> a
bytesToNum = Prelude.foldl unstep 0
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

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

-- Parser of MIDI
midiP :: Parsec [Word8] u Midi
midiP = do
  header <- midiHeaderP
  tracks <- many1 midiTrackP
  return $ Midi header tracks


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
    -- [NOTICE] this `chunkTypeP` is different from MIDI Track's
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
      return 6 -- TODO Change but I think always 6

    -- Parser of MIDI Format
    midiFormatP :: Parsec [Word8] u MidiFormat
    midiFormatP = do
      -- Get format in [Word8]
      formatBytes <- takeP 2
      -- Convert [Word8] to Int
      let formatInt = bytesToNum formatBytes :: Int
      case formatInt of
        0 -> return MidiFormat0
        1 -> return MidiFormat1
        2 -> return MidiFormat2
        _ -> unexpected ("Unexpected format number: " ++ show formatInt)

    -- Parser of the number of tracks
    tracksNumP :: Parsec [Word8] u Int
    tracksNumP = do
      -- Get the number of tracks in [Word8]
      tracksNumBytes <- takeP 2
      -- Convert [Word8] to Int
      let tracksNum = bytesToNum tracksNumBytes :: Int
      return tracksNum

    -- Parser of Time Unit
    timeUnitP :: Parsec [Word8] u Int
    timeUnitP = do
      -- Get the number of time unit in [Word8]
      timeUnitBytes <- takeP 2
      -- Convert [Word8] to Int
      let timeUnit = bytesToNum timeUnitBytes :: Int
      return timeUnit

-- Parser of MIDI Track
midiTrackP :: Parsec [Word8] u MidiTrack
midiTrackP = do
  -- Consume (=validate) Chunk Type
  chunkTypeP
  -- Get Data Length (I think I won't use it)
  dataLength <- dataLengthP
  -- Get Deleta-times and events
  deltaTimesAndEvents <- deltaTimesAndEventsP

  return $ MidiTrack deltaTimesAndEvents
  where
    -- Parser(=consumer) of MIDI Track Chunk type
    -- [NOTICE] this `chunkTypeP` is different from MIDI Header's
    chunkTypeP :: Parsec [Word8] u ()
    chunkTypeP = do
      satisfyListHead (==0x4D)
      satisfyListHead (==0x54)
      satisfyListHead (==0x72)
      satisfyListHead (==0x6B)
      return ()

    -- Parser of Data Length (I think I won't use it)
    dataLengthP :: Parsec [Word8] u Int
    dataLengthP = bytesToNum <$> takeP 4

    -- Parser of (Deleta-Time, MIDI Event)
    deltaTimesAndEventsP :: Parsec [Word8] u [(DeltaTime, MidiEvent)]
    deltaTimesAndEventsP = do
      -- Get delta-time
      deltaTime <- deltaTimeP
      -- TODO implement
      let midiEvent = NoteOn (-1) (-1) (-1)
      return [(deltaTime, midiEvent)]

    -- Parser of Delta Time
    deltaTimeP :: Parsec [Word8] u DeltaTime
    deltaTimeP = do
      t <- _7bitsListToNum <$> deltaTime7bitsListP
      return $ DeltaTime t
      where
        -- Get sequence delta-time bytes (7bits)
        deltaTime7bitsListP :: Parsec [Word8] u [Word8]
        deltaTime7bitsListP = do
          b <- headP
          if (testBit b 8)
            then ((clearBit b 8) :) <$> deltaTime7bitsListP
            else return [b]

        -- Convert [Word8 as 7bits] to (Num a => a)
        -- hint: http://stackoverflow.com/a/31208816/2885946
        _7bitsListToNum :: (Num a, Bits a) => [Word8] -> a
        _7bitsListToNum = Prelude.foldl unstep 0
          where
            unstep a b = a `shiftL` 7 .|. fromIntegral b


main :: IO ()
main = do
  bytes <- unpack <$> BS.readFile "./yokoso.mid"

  -- MIDI Header
  parseTest midiHeaderP bytes

  -- MIDI
  parseTest midiP bytes

  return ()
