# MIDI File Reader - Haskell

## What is this?

MIDI File Reader written in Haskell.
Target MIDI is SMF(=Standard MIDI Format).

## Main Features

  * written in Haskell
  * parsed by Parsec

## How to use

### Build & Execution

```sh
$ stack build
$ stack exec midi-reader-haskell-exe test.mid
```
`test.mid` have to be replace with your MIDI FILE

### Sample output

An output is like the following.

```
Header
MidiHeader {getMidiFormat = MidiFormat1, getTracksNum = 4, getTimeUnit = 480}
Track1
(DeltaTime 0,MetaEvent (MeasureEvent {getNn = 4, getDd = 2, getCc = 24, getBb = 8}))
(DeltaTime 0,MetaEvent (KeyEvent {getSharpOrFlat = Flat, getSharpOrFlatNum = 127, getKey = MajorKey}))
(DeltaTime 0,MetaEvent (KeyEvent {getSharpOrFlat = Sharp, getSharpOrFlatNum = 0, getKey = MajorKey}))
(DeltaTime 0,MetaEvent (KeyEvent {getSharpOrFlat = Sharp, getSharpOrFlatNum = 0, getKey = MajorKey}))
(DeltaTime 0,MetaEvent (TempoEvent 352942))
(DeltaTime 0,ControlChange {first = 176, second = 121, third = 0})
(DeltaTime 0,ControlChange {first = 192, second = 0, third = 0})
(DeltaTime 6151,ControlChange {first = 192, second = 100, third = 0})
(DeltaTime 10,ControlChange {first = 192, second = 64, third = 0})
(DeltaTime 91,ControlChange {first = 192, second = 0, third = 0})
(DeltaTime 93,ControlChange {first = 192, second = 0, third = 0})
(DeltaTime 16289,ControlChange {first = 192, second = 1, third = 0})
(DeltaTime 3840,NoteOn {getChannel = 0, getPitch = 65, getVelocity = 80})
(DeltaTime 227,NoteOn {getChannel = 0, getPitch = 65, getVelocity = 0})
(DeltaTime 13,NoteOn {getChannel = 0, getPitch = 64, getVelocity = 80})
(DeltaTime 227,NoteOn {getChannel = 0, getPitch = 64, getVelocity = 0})
(DeltaTime 13,NoteOn {getChannel = 0, getPitch = 65, getVelocity = 80})
(DeltaTime 227,NoteOn {getChannel = 0, getPitch = 65, getVelocity = 0})
(DeltaTime 13,NoteOn {getChannel = 0, getPitch = 67, getVelocity = 80})
(DeltaTime 455,NoteOn {getChannel = 0, getPitch = 67, getVelocity = 0})
(DeltaTime 25,NoteOn {getChannel = 0, getPitch = 65, getVelocity = 80})
(DeltaTime 227,NoteOn {getChannel = 0, getPitch = 65, getVelocity = 0})
(DeltaTime 13,NoteOn {getChannel = 0, getPitch = 67, getVelocity = 80})
(DeltaTime 227,NoteOn {getChannel = 0, getPitch = 67, getVelocity = 0})
(DeltaTime 13,NoteOn {getChannel = 0, getPitch = 69, getVelocity = 80})
(DeltaTime 455,NoteOn {getChannel = 0, getPitch = 69, getVelocity = 0})
(DeltaTime 25,NoteOn {getChannel = 0, getPitch = 70, getVelocity = 80})
(DeltaTime 227,NoteOn {getChannel = 0, getPitch = 70, getVelocity = 0})
...
```

(The outptut is very long. So it is elided.)
