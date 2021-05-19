{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import Data.Aeson
import GHC.Generics
import Data.Map as M (Map, lookup)

data Transition = Transition {
    read_ :: String,
    to_state :: String,
    write :: String,
    action :: String
} deriving (Show)

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

instance FromJSON Configuration

data Tape = Tape {
    left :: [Char],
    right :: [Char],
    symbol :: Char
}

data TMachine = TMachine {
    tape :: Tape,
    cfg :: Configuration,
    state :: String,
    stuck :: Bool
}
