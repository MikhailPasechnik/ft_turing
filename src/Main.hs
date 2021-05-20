{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Text.Printf
import Data.List as L
import System.Exit
import System.Environment
import Debug.Trace
import Data.Map as M (Map, lookup, keys, elems, assocs)
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.IO
import Control.Monad

import Models

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

initTMachine :: Configuration -> String -> TMachine
initTMachine config tapeString = TMachine {
    cfg = config,
    tape = initTape tapeString,
    state = initial config,
    stuck = False
}

currentSymbol tm = symbol (tape tm)
blankSymbol tm = head (blank (cfg tm))

pprintTMachine :: TMachine -> IO ()
pprintTMachine tm = putStrLn (printf "|%s\ESC[38;2;255;0;0m%c\ESC[0m%s| %s <--- %s" (left t) (symbol t) (right t) (state tm) message)
    where
        message = if stuck tm then "PROGRAM STUCK" else "PROGRAM FINISHED" :: String
        t = tape tm

reprTMachine :: TMachine -> String    
reprTMachine tm =
    case M.lookup (state tm) (transitions config) of
        Just transition -> do
            printf "|%s\ESC[38;2;255;0;0m%c\ESC[0m%s| (%s, %c) -> (%s, %s, %s)" (left t) (symbol t) (right t) (state tm) (symbol t)
                (to_state (head (filter (\i -> head (read_ i) == symbol t) transition)))
                (write (head (filter (\i -> head (read_ i) == symbol t) transition)))
                (action (head (filter (\i -> head (read_ i) == symbol t) transition)))
        Nothing -> do
            printf "|%s\ESC[38;2;255;0;0m%c\ESC[0m%s|" (left t) (symbol t) (right t)
    where
        config = cfg tm
        t = tape tm

nextTransition :: TMachine -> Maybe Transition
nextTransition tm = case mindex of 
        Just index -> Just (trs!!index)
        _ -> Nothing 
    where
        mindex = L.findIndex (\t -> head (read_ t) == s) trs
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

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

run :: TMachine -> TMachine
run tm | elem (state tm) (finals config) || stuck tm = trace (reprTMachine tm) tm
              | otherwise = trace (reprTMachine tm) (run (execute tm))
    where
        config = cfg tm

execute :: TMachine -> TMachine
execute tm  | finished = TMachine{
                tape = tape tm,
                state = state tm,
                cfg = cfg tm,
                stuck = False
              }
            | valid = TMachine{
                tape = applyTransition next (blankSymbol tm) (tape tm),
                state = to_state next,
                cfg = cfg tm,
                stuck = False
              }
            | otherwise = TMachine{
                tape = tape tm,
                state = state tm,
                cfg = cfg tm,
                stuck = True
              }
            where
                config = cfg tm
                finished = state tm `elem` finals config
                mnext = nextTransition tm
                valid = isJust mnext
                -- valid = not (isNothing mnext)
                next = case mnext of Just next -> next

checkFile filename = do
    fileExist <- doesFileExist filename
    if fileExist then do
        filePermissions <- getPermissions filename
        if readable filePermissions then
            return True
        else return False
    else return False

checkConfig :: Configuration -> String
checkConfig config | not alphabetLenOne = "Alphabet must consists of strings with length of one!"
                   | not blankInAlphabet = printf "Blank character '%s' not in alphabet!" (blank config)
                   | not initalInState = "Initial state not in states!"
                   | not allFinalsInStates = "Not all final states in states!"
                   | not allTransitionsKeysInStates = "Not all transitions keys in states!"
                   | not allReadInAlphabet = "Not all transitions read symbol in alphabet!"
                   | not allWriteInAlphabet = "Not all transitions read symbol in alphabet!"
                   | not allToStateInStates = "Not all transition to state def in states!"
                   | not actionOk = "Some action in transition is not RIGHT and not LEFT!"
                   | otherwise = ""
                   where
                       alphabetLenOne = all (\x -> length x == 1) (alphabet config)
                       blankInAlphabet = elem (blank config) (alphabet config)
                       initalInState = elem (initial config) (states config)
                       allFinalsInStates = all (\e -> elem e (states config)) (finals config)
                       allTransitionsKeysInStates = all (\e -> elem e (states config)) (keys (transitions config))  
                       allReadInAlphabet = all (\e -> (all (\ee -> elem (read_ ee) (alphabet config)) e)) (elems (transitions config))
                       allWriteInAlphabet = all (\e -> (all (\ee -> elem (write ee) (alphabet config)) e)) (elems (transitions config))
                       allToStateInStates = all (\e -> (all (\ee -> elem (to_state ee) (states config)) e)) (elems (transitions config))
                       actionOk = all (\e -> (all (\ee -> elem (action ee) (["RIGHT", "LEFT"])) e)) (elems (transitions config))

checkInput :: Configuration -> String -> String
checkInput config input | not fromAlphabet = "Input must consist of 'alphabet' symbols"
                        | not withoutBlank = "'Blank' symbol is forbidden in input" 
                        | otherwise = ""
    where
        fromAlphabet = all (\i -> i `elem` join (alphabet config)) input
        withoutBlank = all (\i -> i `notElem` blank config) input

printProgramConfig :: Configuration -> IO ()
printProgramConfig config = do
    let leftPadding = (39 - ((length (name config)) `div` 2))
    let rightPadding = (39 - ((length (name config)) - ((length (name config)) `div` 2)))

    putStrLn (replicate 80 '*')
    putStrLn ("*" ++ (replicate 78 ' ') ++ "*")
    putStrLn ("*" ++ (replicate leftPadding ' ') ++ (name config) ++ (replicate rightPadding ' ') ++ "*")
    putStrLn ("*" ++ (replicate 78 ' ') ++ "*")
    putStrLn (replicate 80 '*')

    putStrLn (printf ("Alphabet: %s") (show (alphabet config)))
    putStrLn (printf ("Blank: \"%s\"") (blank config))
    putStrLn (printf ("States: %s") (show (states config)))
    putStrLn (printf ("Initial: \"%s\"") (initial config))
    putStrLn (printf ("Finals: %s") (show (finals config)))
    forM_ (assocs (transitions config)) (\i -> forM_ ((snd i)) (\j -> putStrLn (printf "(%s, %s) -> (%s, %s, %s)" (fst i) (read_ j) (to_state j) (write j) (action j))))
    putStrLn (replicate 80 '*')

printUsage :: IO ()
printUsage = do
    putStrLn "usage: ft_turing [-h] jsonfile input\n"
    putStrLn "positional arguments:"
    putStrLn "   jsonfile                 json description of the machine\n"
    putStrLn "   input                    input of the machine\n"
    putStrLn "optional arguments:"
    putStrLn "   -h, --help               show this help message and exit"

main :: IO ()
main = do
    args <- getArgs -- Read program arguments to 'args' variable
    if elem "-h" args || elem "--help" args then do
        printUsage
        exitSuccess
    else return ()
    case args of -- Switch based on content of the 'args'
        [configFile, tapeText] -> do
            fileOk <- checkFile configFile
            if fileOk then return ()
            else do
                putStrLn "Something wrong with the config file"
                exitWith (ExitFailure 1)

            configFileBuffer <- B.readFile configFile
            let mconfig = decode configFileBuffer :: Maybe Configuration
            case mconfig of
                Just config -> do
                    let configError = checkConfig config
                    if null configError then return ()
                    else do
                        putStrLn configError
                        exitWith (ExitFailure 1)

                    let inputError = checkInput config tapeText
                    if null inputError then return ()
                    else do
                        putStrLn inputError
                        exitWith (ExitFailure 1)

                    printProgramConfig config
                    let machine = initTMachine config tapeText
                    let f = run machine
                    pprintTMachine f
                _ -> do
                    putStrLn "Something wrong with config"
        _ -> do -- Switch default
            printUsage
            exitWith (ExitFailure 1)
