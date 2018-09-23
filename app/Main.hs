module Main where

import           ReaderWriter
import           StateWriter
main :: IO ()
main = do
    testReducer
    stateReducer
