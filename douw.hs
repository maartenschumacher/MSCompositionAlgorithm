
data Octave = Octave Int deriving (Show)
data Pitch = ANatural | ASharp | BNatural | CNatural | CSharp 
	| DNatural | DSharp | ENatural | FNatural | FSharp | GNatural | GSharp
	deriving (Read, Show, Eq, Ord, Bounded, Enum)

data Note = Note Octave Pitch deriving (Show)

data Harmony = Harmony [Pitch]

data Chord = Chord [Note]

data VoiceLeading = VoiceLeading [Note]

data PitchFunction = PitchFunction Int
data HarmonyStructure = HarmonyStructure [PitchFunction]

--harmonize :: HarmonyStructure -> Pitch -> Harmony

--chromaticLine :: [Pitch]

-- use enums for octaves and pitches

--arpeggiate :: [Chord] -> [Note]

--arrange :: VoiceLeading -> [Harmony] -> [Chord]


