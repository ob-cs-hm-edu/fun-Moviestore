module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Moviestore

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr $ showMoviestore myMovieStore

