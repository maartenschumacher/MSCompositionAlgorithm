import Data.List
import Data.Ord

data Octave = Octave Int deriving (Show, Eq, Ord)
octave :: Int -> Octave
octave n
	| n < 0 || n > 10 = error "Octave out of bounds"
	| otherwise = Octave n

data Pitch = CNatural | CSharp | DNatural | DSharp | ENatural | FNatural 
	| FSharp | GNatural | GSharp | ANatural | ASharp | BNatural
	deriving (Read, Show, Eq, Ord, Bounded, Enum)

type Note = (Pitch, Octave)

totalPitches :: Int
totalPitches = fromEnum (maxBound :: Pitch) + 1

type Interval = Int

addPitch :: Pitch -> Interval -> Pitch
addPitch = flip turn

addNote :: Note -> Interval -> Note
addNote (p, Octave o) n = (addPitch p n, octave $ o + (addedOctaves p n))

interval :: Pitch -> Pitch -> Interval
interval a b
	| backward < forward = negate backward
	| otherwise = forward
	where 
		findPitchBy = findPitch b a
		forward = findPitchBy next
		backward = findPitchBy prev


chromatic :: [Pitch]
chromatic = [ANatural .. GSharp]

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add mod x y = (x + y + mod) `rem` mod

findPitch :: Pitch -> Pitch -> (Pitch -> Pitch) -> Int
findPitch p1 p2 f
	| p1 == p2 = 0
	| otherwise = 1 + (findPitch p1 (f p2) f)

addedOctaves :: Pitch -> Int -> Int
addedOctaves p n = floor $ realToFrac (fromEnum p + n) / realToFrac totalPitches

-- Harmony

type PitchFunction = Int
type Harmony = [Pitch]
type HarmonyStructure = [PitchFunction]

harmonize :: HarmonyStructure -> Pitch -> Harmony
harmonize hs p = map (addPitch p) hs

invert :: HarmonyStructure -> Int -> HarmonyStructure
invert hs n = sort $ map (flip mod totalPitches) $ map (flip(-) (hs !! n)) hs

inversions :: HarmonyStructure -> [HarmonyStructure]
inversions hs = map (invert hs) (indexes hs)

doTheThing :: HarmonyStructure -> [Pitch] -> [Harmony]
doTheThing hs ps = zipWith harmonize (cycle $ inversions hs) ps


-- arranging

type Chord = [Note]

arrange :: Chord -> Harmony -> Chord
arrange [] _ = [] -- it assumes that length chord == length harmony
arrange (n:ns) ps = (p,o) : arrange ns (delete p ps)
	where
		(p,o) = findNearestNote n ps

findNearestNote :: Note -> [Pitch] -> Note
findNearestNote (n,o) ps = addNote (n,o) $ closestToZero $ map (interval n) ps

sortChord :: Chord -> Chord
sortChord = sortBy (comparing snd) . sort


--globals

indexes :: [a] -> [Int]
indexes xs = [0 .. (length xs - 1)]

closestToZero :: (Ord a, Num a) => [a] -> a
closestToZero [] = error "Doesn't work on empty list"
closestToZero (x:[]) = x
closestToZero (x:xs) = absMin x (closestToZero xs)

absMin :: (Ord a, Num a) => a -> a -> a
absMin a b = if (abs a) < (abs b) then a else b
