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
    state = initial config
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

run :: TMachine -> TMachine
-- TODO: Check not == "HALT" but (state tm) in config.finals
run tm | state tm == "HALT" = trace (reprTMachine tm) tm
       | otherwise = trace (reprTMachine tm) (run . execute) tm

execute :: TMachine -> TMachine
execute tm  | finished = TMachine{
                tape = tape tm,
                state = state tm,
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
                -- TODO: Check not == "HALT" but (state tm) in config.finals
                finished = state tm == "HALT"
                mnext = nextTransition tm
                valid = not (isNothing mnext)
                next = case mnext of Just next -> next
                r =  execute TMachine{
                        tape=tape tm,
                        cfg=cfg tm,
                        state=state tm
                    }

checkFile :: FilePath -> IO Bool
checkFile filename = do
    fileExist <- doesFileExist filename
    if fileExist then do
        filePermissions <- getPermissions filename
        if readable filePermissions then
            return True
        else do
            putStrLn "Permission denied"
            return False
    else do
        putStrLn "File does not exist"
        return False

printProgramName :: String -> IO ()
printProgramName name = do
    let leftPadding = (39 - ((length name) `div` 2))
    let righPadding = (39 - ((length name) - ((length name) `div` 2)))

    putStrLn (replicate 80 '*')
    putStrLn ("*" ++ (replicate 78 ' ') ++ "*")
    putStrLn ("*" ++ (replicate leftPadding ' ') ++ name ++ (replicate righPadding ' ') ++ "*")
    putStrLn ("*" ++ (replicate 78 ' ') ++ "*")
    putStrLn (replicate 80 '*')

-- Check that user input take consists of alphabet only and does't contain any blank symbol
-- Check that all transitions op to_state fields in states
-- Check that all transitions op read/write symbols is 1 length and present in alphabet
-- Check that all transitions op action is "RIGHT" or ""

checkConfig :: Configuration -> IO Bool
checkConfig config = do
    if all (\x -> length x == 1) (alphabet config)
        && elem (blank config) (alphabet config)
        && elem (initial config) (states config)
        && all (\e -> elem e (states config)) (finals config) then
        -- && all (\e -> (all (\ee -> elem (read ee) (alphabet config)) (snd e))) (assocs (transitions config)) then
        return True
    else return False

main :: IO ()
main = do
    args <- getArgs -- Read program arguments to 'args' variable
    case args of -- Switch based on content of the 'args'
        [configFile, tapeText] -> do
            fileOk <- checkFile configFile
            if fileOk then return ()
            else exitWith (ExitFailure 1)

            configFileBuffer <- B.readFile configFile
            let mconfig = decode configFileBuffer :: Maybe Configuration
            case mconfig of
                Just config -> do
                    configOk <- checkConfig config
                    if configOk then return ()
                    else exitWith (ExitFailure 1) 

                    printProgramName (name config)
                    let machine = initTMachine config tapeText
                    let f = run machine
                    pprintTMachine f
                _ -> do
                    putStrLn "Json file parse error"
        _ -> do -- Switch default
            putStrLn "Wrong number of arguments!"
            exitWith (ExitFailure 1)
