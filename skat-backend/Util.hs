{-# LANGUAGE FlexibleContexts #-}

module Util (
    println,
    maxElem,
    shuffle,
    assert,
    slice,
    maybeToError
) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Array.IO
import Data.List as List
import System.Random

println :: MonadIO m => String -> m ()
println = liftIO . putStrLn

maxElem :: Ord a => [a] -> Maybe a
maxElem = List.foldr (max . Just) Nothing

shuffle :: MonadIO m => [a] -> m [a]
shuffle xs = liftIO $ do
    ar <- array n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i, n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = List.length xs
    array :: Int -> [a] -> IO (IOArray Int a)
    array m ys =  newListArray (1, m) ys

assert :: Bool -> String -> Either String ()
assert True _ = return ()
assert False s = throwError s

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

maybeToError :: MonadError String m => String -> Maybe a -> m a
maybeToError message = maybe (throwError message) return
