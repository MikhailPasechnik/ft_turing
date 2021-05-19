{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Text.Printf
import Data.List as L
import System.Exit
import System.Environment
import Debug.Trace
import Data.Map as M (Map, lookup, keys, elems, assocs)
import qualified Data.ByteString.Lazy as B
import System.Directory
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
pprintTMachine tm = putStrLn (printf "tape: %s|%c|%s" (left t) (symbol t) (right t))
    where t = tape tm

reprTMachine :: TMachine -> String    
reprTMachine tm = printf "state: %s tape: %s<%c>%s" (state tm) (left t) (symbol t) (right t)
    where t = tape tm

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

run :: Configuration -> TMachine -> TMachine
run config tm | elem (state tm) (finals config) || stuck tm = trace (reprTMachine tm) tm
              | otherwise = trace (reprTMachine tm) (run (config) (execute config tm))

execute :: Configuration -> TMachine -> TMachine
execute config tm  | finished = TMachine{
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
                finished = elem (state tm) (finals config)
                mnext = nextTransition tm
                valid = not (isNothing mnext)
                next = case mnext of Just next -> next

checkFile filename = do
    fileExist <- doesFileExist filename
    if fileExist then do
        filePermissions <- getPermissions filename
        if readable filePermissions then
            return True
        else return False
    else return False

checkConfig :: Configuration -> IO Bool
checkConfig config = do
    if all (\x -> length x == 1) (alphabet config)
        && elem (blank config) (alphabet config)
        && elem (initial config) (states config)
        && all (\e -> elem e (states config)) (finals config)
        && all (\e -> elem e (states config)) (keys (transitions config))
        && all (\e -> (all (\ee -> elem (read_ ee) (alphabet config)) e)) (elems (transitions config))
        && all (\e -> (all (\ee -> elem (write ee) (alphabet config)) e)) (elems (transitions config)) 
        && all (\e -> (all (\ee -> elem (to_state ee) (states config)) e)) (elems (transitions config))
        && all (\e -> (all (\ee -> elem (action ee) (["RIGHT", "LEFT"])) e)) (elems (transitions config)) then
        return True
    else return False

checkInput :: Configuration -> String -> IO Bool
checkInput config input = do
    if all (\i -> elem i (join (alphabet config))) input then
        return True
    else return False

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
        exitWith ExitSuccess
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
                    configOk <- checkConfig config
                    if configOk then return ()
                    else do
                        putStrLn "Something wrong with config"
                        exitWith (ExitFailure 1) 

                    inputOk <- checkInput config tapeText
                    if inputOk then return ()
                    else do
                        putStrLn "Something wrong with input"
                        exitWith (ExitFailure 1)

                    printProgramConfig config
                    let machine = initTMachine config tapeText
                    let f = run config machine
                    pprintTMachine f
                _ -> do
                    putStrLn "Something wrong with config"
        _ -> do -- Switch default
            printUsage
            exitWith (ExitFailure 1)
