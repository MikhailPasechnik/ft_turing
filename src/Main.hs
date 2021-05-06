--Use the OverloadedStrings language extension to represent strings as ByteString
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
-- Json parsing library https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html
import Data.Aeson
import Text.Printf
import Data.List as L
import System.Exit
import System.Environment
import GHC.Generics
import Debug.Trace
import Data.Map as M (Map, lookup)
import qualified Data.ByteString.Lazy as B

data Transition = Transition {
    read_ :: String,
    to_state :: String,
    write :: String,
    action :: String
} deriving (Show)
-- We expect a JSON object, so we fail at any non-Object value.
instance FromJSON Transition where
    parseJSON (Object v) = do
        read_ <- v .: "read"
        to_state <- v .: "to_state"
        write <- v .: "write"
        action <- v .: "action"
        return Transition{
            read_= read_,
            to_state = to_state,
            write = write,
            action = action
        }

data Configuration = Configuration {
    name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: M.Map String [Transition]
} deriving (Generic, Show)

--    left      current    right 
-- [a,b,c,d,e]     a    [q,e,r,t,y]
data Tape = Tape {
    left :: [Char],
    right :: [Char],
    symbol :: Char
}

initTape :: [Char] -> Tape
initTape text = Tape{
    symbol = head text,
    left = [],
    right = tail text
}
moveTape :: Transition -> Char -> Tape -> Tape
moveTape tr blank tape = case action tr of
    "LEFT" -> Tape{
        left = if null l then [] else init l,
        right = s : r,
        symbol = if null l then blank else last l
    }
    "RIGHT"  -> Tape{
        left = l ++ [s],
        right = if null r then [] else tail r,
        symbol = if null r then blank else head r
    }
    where 
        l = left tape
        r = right tape
        s = symbol tape
        

type State = String

data TMachine = TMachine {
    tape :: Tape,
    cfg :: Configuration,
    state :: State
}
initTMachine :: Configuration -> String -> TMachine
initTMachine config tapeString = TMachine {
    cfg = config,
    tape = initTape tapeString,
    state = initial config
}
currentSymbol tm = symbol (tape tm)
blankSymbol tm = head (blank (cfg tm))
pprintTMachine :: TMachine -> IO ()
pprintTMachine tm = putStrLn (printf "tape: %s|%c|%s" (left t) (symbol t) (right t))
    where t = tape tm
reprTMachine tm = printf "state: %s tape: %s<%c>%s" (state tm) (left t) (symbol t) (right t)
    where t = tape tm
nextTransition :: TMachine -> Maybe Transition
nextTransition tm = case i of 
        Just index -> Just (trs!!index)
        _ -> Nothing 
    where
        i = L.findIndex (\t -> head (read_ t) == s) trs
        trs = case mtrs of Just trs -> trs
        mtrs = M.lookup (state tm) (transitions (cfg tm))
        s = currentSymbol tm
applyTransition :: Transition -> Char -> Tape -> Tape

applyTransition tr blank tape = do
    let t = Tape {
        left = left tape,
        right = right tape,
        symbol = head (write tr)
    }
    moveTape tr blank t
-- applyTransition tr blank tape = trace (printf "applyTransition: s: %c b: %c a: %s => %s" (head (write tr)) blank (action tr) (left tape)) moveTape (action tr) blank Tape {
--         left = left tape,
--         right = right tape,
--         symbol = head (write tr)
--     }
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

run tm | state tm == "HALT" = trace (reprTMachine tm) tm
       | otherwise = trace (reprTMachine tm) (run . execute) tm

execute :: TMachine -> TMachine
execute tm  | finished = TMachine{
                tape = tape tm,
                state = "HALT",
                cfg = cfg tm
              }
            | valid = TMachine{
                tape = applyTransition next (blankSymbol tm) (tape tm),
                state = to_state next,
                cfg = cfg tm
              }
            | otherwise = TMachine{
                tape = tape r,
                state = state r,
                cfg = cfg r
              }
            where 
                finished = state tm == "HALT"
                mnext = nextTransition tm
                valid = not (isNothing mnext)
                next = case mnext of  Just next -> next
                r =  execute TMachine{
                        tape=tape tm,
                        cfg=cfg tm,
                        state=state tm
                    }

--  case mnext of
--     Just next ->
--         case state tm of
--             "HALT" -> tm
--             _ -> do
--                 trace nextState execute TMachine{
--                     tape=applyTransition next (blankSymbol tm) (tape tm),
--                     cfg=cfg tm,
--                     state=nextState
--                 }
--                 where
--                     mnext = case state tm of 
--                         "HALT" -> trace "where..\n" Nothing
--                         _ -> trace "where..\n" nextTransition tm           
--                     nextState = case mnext of 
--                         Just next -> to_state next
--                         _ -> "HALT"
--     _ -> trace (reprTMachine tm) TMachine{
--             tape=tape tm,
--             cfg = cfg tm,
--             state="HALT"
--         }
--     where
--         mnext = case state tm of 
--             "HALT" -> Nothing
--             _ -> nextTransition tm

instance FromJSON Configuration

-- Alphabet elements length is 1 
isAlphabetValid :: Configuration -> Bool
isAlphabetValid config = all (\x -> length x == 1) (alphabet config)
-- Blank symbol length is 1 and present in alphabet
isBlankValid :: Configuration -> Bool
isBlankValid config = length (blank config) == 1 && elem (blank config) (alphabet config)
-- States must contain at least "HALT" and initial must be in states and all finals must be in states
isStatesValid :: Configuration -> Bool
isStatesValid config = elem "HALT" (states config) && elem (initial config) (states config) &&
    all (\e -> elem e (states config)) (finals config)

main :: IO ()
main = do
    args <- getArgs -- Read program arguments to 'args' variable
    case args of -- Switch based on content of the 'args'
        [configFile, tapeText] -> do -- Add 'do' for multiline expression
            putStrLn (printf "Turing machine starting with config %s and tape %s" configFile tapeText)
            input <- B.readFile configFile
            let mm = decode input :: Maybe Configuration
            case mm of
                Just config -> do
                    let m = initTMachine config tapeText
                    let f = run m
                    pprintTMachine f
                _ -> do
                    putStrLn "dead"
        _ -> do -- Switch default
            putStrLn "Wrong numper of arguments!"
            exitWith (ExitFailure 1)
