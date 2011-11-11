module Main where

import System.Environment (getArgs)
import Miner (mine)

main = do
    [uriString, user, password] <- getArgs
    mine uriString user password
