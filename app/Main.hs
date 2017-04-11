module Main where

import           MidiParser

import           Control.Monad
import           Data.ByteString.Lazy as BS
import           System.Environment
import           Text.Parsec          (runParser)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      -- Read bytes from the file path
      bytes <- unpack <$> BS.readFile filePath

      -- Run parser
      case runParser midiP Nothing "MIDI(SMF) Parsing" bytes of
        Right (midi)  -> do
          -- Get midi header and tracks
          let Midi header tracks = midi

          -- Print header
          Prelude.putStrLn "Header"
          print header

          -- Print tracks
          forM_ (Prelude.zip [1..] tracks) $ \(num, track) -> do
            Prelude.putStrLn $ "Track" ++ show num
            let MidiTrack xs = track
            mapM_ print xs

        Left (excep)  -> print excep

    _          -> do
      Prelude.putStrLn "Usage: ./midi-reader <file-path>"
