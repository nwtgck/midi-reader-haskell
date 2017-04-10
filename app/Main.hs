module Main where


import           Data.Word8
import           Text.Parsec

-- MIDI File
data Midi = Midi {
             getMidiHeader :: MidiHeader
           , getTracks     :: [MidiTrack]
           }
           deriving (Show, Eq)

-- MIDI Header
data MidiHeader = MidiHeader {
                    getMidiFormat :: MidiFormat
                  , getTracksNum  :: Integer
                  , getTimeUnit   :: Integer -- This is delta time
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

main :: IO ()
main = return ()
