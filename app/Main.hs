{-# LANGUAGE MultiWayIf #-}
module Main where



import           Control.Applicative
import           Control.Monad
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
                | ControlChange {first :: Word8, second :: Word8, third :: Word8}
                | MetaEvent MetaEvent
                deriving (Show, Eq)

-- Meta Event
data MetaEvent =  CommentEvent ByteString
                | CopyrightEvent ByteString
                | TrackNameEvent ByteString
                | InstrumentNameEvent ByteString
                | LyricsEvent ByteString
                | TempoEvent Int
                | MeasureEvent {getNn :: Word8, getDd :: Word8, getCc :: Word8, getBb :: Word8}
                | KeyEvent {getSharpOrFlat :: SharpOrFlat, getSharpOrFlatNum :: Int, getKey :: Key}
                | FinishTrackEvent
                deriving (Show, Eq)

-- Major key or Minor Key
data Key = MajorKey | MinorKey deriving (Show, Eq)

data SharpOrFlat = Sharp | Flat deriving (Show, Eq)

-- Available Running Status
data RunningStatus =  RunningStatus {getPrevStatusByte :: Word8} deriving (Show, Eq)

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
    (x:xs) -> do
      setInput xs
      pos <- getPosition
      let column = sourceColumn pos
          newPos = setSourceColumn pos (column+1)
      setPosition newPos
      return x
    []     -> unexpected "No head in headP"

-- Push head parser (Rewind)
pushHeadP :: a -> Parsec [a] u ()
pushHeadP h = do
  input <- getInput
  setInput(h:input)
  pos <- getPosition
  let column = sourceColumn pos
      newPos = setSourceColumn pos (column-1)
  setPosition newPos

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
midiP :: Parsec [Word8] (Maybe RunningStatus) Midi
midiP = do
  header <- midiHeaderP
  tracks <- midiTracksP
  -- track <- midiTrackP

  return $ Midi header tracks
  -- return $ Midi header [track]



-- Parser for MIDI Header
midiHeaderP :: Parsec [Word8] (Maybe RunningStatus) MidiHeader
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
    chunkTypeP :: Parsec [Word8] (Maybe RunningStatus) ()
    chunkTypeP = do
      satisfyListHead (==0x4D)
      satisfyListHead (==0x54)
      satisfyListHead (==0x68)
      satisfyListHead (==0x64)
      return ()

    -- Parser of data length
    dataLengthP :: Parsec [Word8] (Maybe RunningStatus) Int
    dataLengthP = do
      dataLengthBytes <- takeP 4
      return 6 -- TODO Change but I think always 6

    -- Parser of MIDI Format
    midiFormatP :: Parsec [Word8] (Maybe RunningStatus) MidiFormat
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
    tracksNumP :: Parsec [Word8] (Maybe RunningStatus) Int
    tracksNumP = do
      -- Get the number of tracks in [Word8]
      tracksNumBytes <- takeP 2
      -- Convert [Word8] to Int
      let tracksNum = bytesToNum tracksNumBytes :: Int
      return tracksNum

    -- Parser of Time Unit
    timeUnitP :: Parsec [Word8] (Maybe RunningStatus) Int
    timeUnitP = do
      -- Get the number of time unit in [Word8]
      timeUnitBytes <- takeP 2
      -- Convert [Word8] to Int
      let timeUnit = bytesToNum timeUnitBytes :: Int
      return timeUnit

-- Parser of MIDI Trakcs
midiTracksP :: Parsec [Word8] (Maybe RunningStatus) [MidiTrack]
midiTracksP = do
  input <- getInput
  if input == []
    then return []
    else (:) <$> midiTrackP <*> midiTracksP


-- Parser of MIDI Track
midiTrackP :: Parsec [Word8] (Maybe RunningStatus) MidiTrack
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
    chunkTypeP :: Parsec [Word8] (Maybe RunningStatus) ()
    chunkTypeP = do
      satisfyListHead (==0x4D)
      satisfyListHead (==0x54)
      satisfyListHead (==0x72)
      satisfyListHead (==0x6B)
      return ()

    -- Parser of Data Length (I think I won't use it)
    dataLengthP :: Parsec [Word8] (Maybe RunningStatus) Int
    dataLengthP = bytesToNum <$> takeP 4

    -- Parser of (Deleta-Time, MIDI Event)
    deltaTimesAndEventsP :: Parsec [Word8] (Maybe RunningStatus) [(DeltaTime, MidiEvent)]
    deltaTimesAndEventsP = do
      -- Get delta-time
      deltaTime <- deltaTimeP
      -- Get MIDI Event
      midiEvent <- midiEventP

      if midiEvent == MetaEvent FinishTrackEvent
        then return [(deltaTime, midiEvent)]
        else do
          -- Recursivly
          deltaTimesAndEvents <- deltaTimesAndEventsP
          return $ (deltaTime, midiEvent) : deltaTimesAndEvents

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
          if (testBit b 7)
            then ((clearBit b 7) :) <$> deltaTime7bitsListP
            else return [b]

        -- Convert [Word8 as 7bits] to (Num a => a)
        -- hint: http://stackoverflow.com/a/31208816/2885946
        _7bitsListToNum :: (Num a, Bits a) => [Word8] -> a
        _7bitsListToNum = Prelude.foldl unstep 0
          where
            unstep a b = a `shiftL` 7 .|. fromIntegral b

    -- Parser of MIDI Event
    midiEventP :: Parsec [Word8] (Maybe RunningStatus) MidiEvent
    midiEventP = do
      headByte <- headP

      statusByte <-
        if testBit headByte 7
          then return headByte
          else do
            runningStatusMay <- getState
            case runningStatusMay of
              Just (RunningStatus b) -> do
                -- Rewind headByte
                pushHeadP headByte
                return b
              Nothing                -> unexpected ("StatusByte is nothing" ++ show headByte)

      if statusByte /= 0xFF
        -- Put RunningStatus
        then putState (Just (RunningStatus statusByte))
        else return ()


      let ev = if
                | 0x80 <= statusByte && statusByte <= 0x8F -> noteOffP (statusByte .&. 0x0F)
                | 0x90 <= statusByte && statusByte <= 0x9F -> noteOnP  (statusByte .&. 0x0F)
                | 0xA0 <= statusByte && statusByte <= 0xEF -> controlChangeP statusByte
                | statusByte == 0xF0 || statusByte == 0xF7 -> unexpected ("StatusByte: SysEx is not implemented yet")
                | statusByte == 0xFF                       -> MetaEvent <$> metaEventP
                | otherwise                                -> unexpected ("Unexpected Status Byte: " ++ show statusByte)

      ev

      where
        noteOffP :: Word8 -> Parsec [Word8] u MidiEvent
        noteOffP channel = do
          pitch    <- headP
          velocity <- headP
          return $ NoteOff channel pitch velocity

        noteOnP :: Word8 -> Parsec [Word8] u MidiEvent
        noteOnP channel = do
          pitch    <- headP
          velocity <- headP
          return $ NoteOn channel pitch velocity

        controlChangeP :: Word8 -> Parsec [Word8] u MidiEvent
        controlChangeP first = do
          second <- headP
          third  <- headP
          return $ ControlChange first second third

        metaEventP :: Parsec [Word8] u MetaEvent
        metaEventP = do
          eventType <- headP
          if
            | eventType == 0x01 -> CommentEvent   <$> byteStringP
            | eventType == 0x02 -> CopyrightEvent <$> byteStringP
            | eventType == 0x03 -> TrackNameEvent <$> byteStringP
            | eventType == 0x04 -> InstrumentNameEvent <$> byteStringP
            | eventType == 0x05 -> LyricsEvent <$> byteStringP
            | eventType == 0x51 -> tempoP
            | eventType == 0x58 -> measureP
            | eventType == 0x59 -> keyP
            | eventType == 0x2F -> finishTrackP
            | otherwise         -> unexpected ("Unexpected Event Type: " ++ show eventType)
          where

            -- Read len, Take text by len
            byteStringP :: Parsec [Word8] u ByteString
            byteStringP = do
              -- Get len
              len   <- headP
              -- Take bytes by len
              bytes <- takeP (fromIntegral len)
              -- Convert to ByteString
              return $ pack bytes

            -- Parser of Finish (0xFF 0x2F 0x00)
            finishTrackP :: Parsec [Word8] u MetaEvent
            finishTrackP = do
              h <- headP
              if h == 0x00
                then return FinishTrackEvent
                else unexpected ("finish number is wrong")

            -- Parser of tempo
            tempoP :: Parsec [Word8] u MetaEvent
            tempoP = do
              _ <- headP -- len (should always =0x03)
              tempoBytes <- takeP 3
              let tempoInt = bytesToNum tempoBytes
              return $ TempoEvent tempoInt

            -- Parser of Mesure
            measureP :: Parsec [Word8] u MetaEvent
            measureP = do
              _ <- headP -- len (should always =0x04)
              nn <- headP
              dd <- headP
              cc <- headP
              bb <- headP
              return $ MeasureEvent nn dd cc bb

            keyP :: Parsec [Word8] u MetaEvent
            keyP = do
              _ <- headP -- len (should always =0x02)
              sfByte <- headP
              mlByte <- headP

              let sf   = if testBit sfByte 7
                          then Flat
                          else Sharp

                  sfNum = fromIntegral $ clearBit sfByte 7


              case mlByte of
                0 -> return $ KeyEvent sf sfNum MajorKey
                1 -> return $ KeyEvent sf sfNum MinorKey
                _ -> unexpected ("ml should be 0 or 1: your input is " ++ show mlByte)


main :: IO ()
main = do
  bytes <- unpack <$> BS.readFile "./yokoso.mid"

  case runParser midiP Nothing "name" bytes of
    Right (midi)  -> do
      let Midi header ts = midi

      Prelude.putStrLn "Header"
      print header

      print $ Prelude.length ts

      forM_ (Prelude.zip [1..] ts) $ \(num, track) -> do
        Prelude.putStrLn $ "Track" ++ show num
        let MidiTrack xs = track
        mapM_ print xs

    Left (excep)  -> print excep

  return ()
