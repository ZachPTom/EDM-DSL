{-
  Name: <Zach Tom>
  Class: CS 252
  Date: <11-6-19>
  Project: EDMDSL
  Description: <Domain specific language for generating electronic dance music>
-}


module EDMDSL (
  Pattern(..),
  Track(..),
  runFile,
  -- runFileBeat,
  showParsedExp
) where

import System.Process
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Data.IORef
import Data.Char

-- Represent sounds, variables, and error messages as strings.
type Sound = String
type Variable = String
type ErrorMsg = String

-- List of strings for parallel playback of tracks
type Parallel = [String]

-- Store is a Map from a string to either a pattern or track
type PStore = Map Variable Pattern
type TStore = Map Variable Track

-- Expressions that will be processed into tracks and compiled into Sonic Pi
data Expression = 
    Var Variable
  | PAssign Variable Pattern
  | TAssign Sound Variable Variable
  | Sequence Expression Expression
  | Val Variable Int Pattern
  | Concat Parallel
  deriving (Show) 

data Pattern = 
    X                                       -- A note 
  | O                                       -- A rest
  | Pattern :| Pattern                      -- :| operator represents a concatenation of patterns
  deriving (Show)

data Track = 
    CreateTrack Sound Pattern               -- One 'sound' and pattern
  | Track := Track                          -- concurrent production operator
  deriving (Show)

data Value =
    IntVal Int                              -- integer for multiplier operand
  deriving (Show)

sPTPath :: String                           -- Command line tool to connect to Sonic Pi Server
sPTPath = "/usr/local/Cellar/sonic-pi-tool/HEAD/bin/"

-- Parser, reads from .edmdsl file
fileP :: GenParser Char st Expression
fileP = do
  prog <- exprP
  eof
  return prog

exprP = do
  e <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing   -> e
    Just e' -> Sequence e e')

exprP' = do
  spaces
  v <- varP
  spaces
  string "="
  spaces
  mult <- optionMaybe multPatterN
  ptrn <- optionMaybe patterN
  prll <- optionMaybe paralleL
  trck <- optionMaybe tracK
  spaces
  return (case v of 
    Var varName -> (case mult of 
      Nothing -> (case ptrn of
        Nothing   -> (case trck of 
            Nothing -> (case prll of 
              Just l -> Concat l)
            Just (istr, Var ptrnname) -> TAssign istr varName ptrnname)
        Just ptrn' -> PAssign varName ptrn')
      Just (IntVal m) -> (case ptrn of
        Just ptrn' -> Val varName m ptrn')))

restSeqP = do
  char ';'
  exprP

patterN = do
  p <- patterN'
  spaces
  rest <- optionMaybe restPatterN
  return (case rest of
      Nothing -> p
      Just p' -> p :| p')
    
patterN' = do
  pStr <- string "X" <|> string "O"
  return (case pStr of
    "X" -> X
    "O" -> O)

--Pattern concatenation operator
multPatterN = do
  numStr <- many1 digit
  spaces
  string "*="
  spaces
  return $ IntVal (read numStr)

restPatterN = do
  string ":|"
  spaces
  patterN

tracK = do
    istr <- many1 (satisfy satisfyHelper)
    spaces
    v <- varP
    return (istr, v)

--Concurrent track compilation operator
paralleL = do
  string ":="
  spaces
  trckName <- many1 letter
  spaces
  more <- optionMaybe trackPrl
  return (case more of
    Just m' -> [trckName] ++ m'
    Nothing -> [trckName])

trackPrl = do
  string ":="
  spaces
  trckName <- many1 letter
  spaces
  more <- optionMaybe trackPrl
  return (case more of
    Just m -> trckName : m
    Nothing -> [trckName])

varP = do
  varStr <- many1 letter
  return $ Var varStr

--Helper function for parsing synth/sample strings for Sonic Pi
satisfyHelper :: Char -> Bool
satisfyHelper x = (((isLetter x) || (isPunctuation x)) && x /= ';')

infixr  7  :|

--Pattern concatenation operator
(*=) :: Int -> Pattern -> Pattern
1 *= p = p
n *= p = p :| (n-1) *= p

--Evaluate functions convert parsed expressions into Tracks and Stores
evaluate :: Expression -> PStore -> TStore -> Either ErrorMsg (Track, PStore, TStore)
evaluate (Val x num ptrn) ps ts = case (Map.insert x (num *= ptrn) ps) of
  y -> Right (CreateTrack "pattern assignment" ptrn, y, ts)

evaluate (PAssign x ptrn) ps ts = case (Map.insert x ptrn ps) of
  y -> Right (CreateTrack "pattern assignment" ptrn, y, ts)

evaluate (TAssign istr trckV ptrnV) ps ts = case (Map.lookup ptrnV ps) of
  (Just y) -> Right (CreateTrack istr y, ps, Map.insert trckV (CreateTrack istr y) ts)
  Nothing -> Left "Pattern not in store"

evaluate (Concat l) ps ts = Right (concatHelper l ts, ps, ts)

evaluate (Sequence e1 e2) ps ts = evaluate e2 ps' ts'
  where Right (v, ps', ts') = evaluate e1 ps ts

--Helper function for concatenation evaluation function
concatHelper :: [String] -> TStore -> Track
concatHelper [x] ts = trck
  where Just trck = Map.lookup x ts 
concatHelper (x:xs) ts = trck := concatHelper xs ts
  where Just trck = Map.lookup x ts

-- Evaluates a program with an initially empty state
run :: Expression -> Either ErrorMsg (Track, PStore, TStore)
run prog = evaluate prog Map.empty Map.empty

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

--Primary function to be called. Parses file and then compiles into Sonic Pi
runFile fileName bpm b = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s1,s2) -> play bpm v b

-----------------------------------------------------------------------------------------------------------------------------------
-------------------------- Sonic Pi Compilation Helper Functions ------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------

--Generate list of list of notes, each list indicating section of consecutive notes
notesList :: Track -> [[Sound]]
notesList trck = case cutNote trck of
					Just ts -> note : notesList ts
					Nothing -> [note]
   where note = extractNote trck

--Obtain the notes from the sequence
extractNote :: Track -> [Sound]
extractNote (CreateTrack s O)=[]
extractNote (CreateTrack s X)= [s]
extractNote (CreateTrack s (X:|ptrn))= [s]
extractNote (CreateTrack s (O:|ptrn))= []
extractNote (CreateTrack s (x:| y)) = extractNote (CreateTrack s x)
extractNote (CreateTrack s X := t)= s : extractNote t
extractNote (CreateTrack s O := t)= extractNote t
extractNote ((CreateTrack s (O:|ptrn)) := t)= extractNote t
extractNote ((CreateTrack s (X:|ptrn)) := t)= s : extractNote t
extractNote ((CreateTrack s (x:|y)) := t)= extractNote (CreateTrack s x) ++  extractNote t
extractNote (t1 := t2) = extractNote t1 ++ extractNote t2

--Remove notes from sequence
cutNote :: Track -> Maybe Track
cutNote (CreateTrack s O)= Nothing
cutNote (CreateTrack s X)= Nothing
cutNote (CreateTrack s (X:|ptrn))= Just (CreateTrack s ptrn)
cutNote (CreateTrack s (O:|ptrn))= Just (CreateTrack s ptrn)
cutNote (CreateTrack s (x:|xs))= Just (CreateTrack s (cutHead (x:|xs)))
cutNote (CreateTrack s X := t)= cutNote t 
cutNote (CreateTrack s O := t)= cutNote t 
cutNote ((CreateTrack s (x:|ptrn)) := t)= case cutNote t of
    Just tf -> Just $ CreateTrack s (cutHead (x:|ptrn)) := tf
    Nothing -> Just $ CreateTrack s (cutHead (x:|ptrn))
cutNote (t1 := t2)= case cutNote t1 of
    Just t1' -> case cutNote t2 of
                 Just t2' -> Just $ t1' := t2'
                 Nothing -> Just t1' 
    Nothing -> cutNote t2 

--Cut out the head note of the pattern
cutHead :: Pattern -> Pattern
cutHead (O :| ptrn) = ptrn
cutHead (X :| ptrn) = ptrn
cutHead (x :| xs) = cutHead x :| xs

--------------------------------------------------------------------------------
------------------------- Compile into Sonic Pi --------------------------------
--------------------------------------------------------------------------------

--Sonic Pi Tool reads written code from a file and sends to the server
play :: Float -> Track  -> Bool -> IO ()
play bpm track b = do        
	writeFile "SonicPi_Code.txt" $ trackToSonicPi (60/bpm) track b
	v <- system $ sPTPath++"sonic-pi-tool eval-file SonicPi_Code.txt"
    	print $ show v

--Extract notes and rests from track
trackToSonicPi :: Float  -> Track -> Bool -> String
trackToSonicPi restLength track b = trackToSonicPiHelper restLength (notesList track) b

--Add rests to file
trackToSonicPiHelper :: Float -> [[Sound]] -> Bool -> String
trackToSonicPiHelper restLength [] b = ""
trackToSonicPiHelper restLength (x:xs) b = produceNotes x b ++ "\tsleep " ++ show restLength ++ "\n" ++ trackToSonicPiHelper restLength xs b

--Add notes to file
produceNotes :: [Sound] -> Bool -> String
produceNotes [] b = ""
produceNotes (x:xs) b = if (b) then "\tsample :" ++ x ++ ", rate: 1\n" ++ produceNotes xs b else "\tsynth :" ++ x ++ "\n" ++ produceNotes xs b