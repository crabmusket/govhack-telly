{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat)
import qualified Data.Set as Set
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Lift as PL
import System.Environment
import System.IO

import Debug.Trace

bstdin :: Producer ByteString IO ()
bstdin = do
    eof <- lift isEOF
    unless eof $ do
        bs <- lift BS.getLine
        yield bs
        bstdin

bsprint :: Consumer ByteString IO ()
bsprint = do
    bs <- await
    lift $ BS8.putStrLn bs
    bsprint

prints :: Show a => Consumer a IO ()
prints = do
    a <- await
    lift $ print a
    prints

passthrough = bstdin >-> bsprint

records :: Monad m => Pipe ByteString (Vector ByteString) m ()
records = do
    bs <- await
    let parsed = Csv.decode Csv.NoHeader (LBS.fromStrict bs)
    case parsed of
        Left err -> records
        Right rs -> if V.null rs
            then records
            else yield (V.head rs) >> records

numFields :: Monad m => Pipe (Vector ByteString) Int m ()
numFields = do
    bs <- await
    yield (V.length bs)
    numFields

selectFields :: (Show a, Monad m) => [Int] -> Pipe (Vector a) (Vector a) m ()
selectFields fs = do
    iv <- await
    let ov = V.create $ do
            v <- MV.new (length fs)
            for_ (zip [0..] fs) (\(i, ix) -> MV.write v i (iv ! ix))
            return v
    yield ov
    selectFields fs

selectField :: Monad m => Int -> Pipe (Vector a) a m ()
selectField f = do
    iv <- await
    yield (iv ! f)
    selectField f

total :: Pipe a Int IO ()
total = PL.evalStateP 0 count where
    count = do
        x <- await
        lift $ modify (+1)
        s <- lift get
        yield s
        count

unique :: (Ord a, Monad m) => Pipe a a m ()
unique = PL.evalStateP Set.empty unique' where
    unique' = do
        el <- await
        prev <- lift get
        when (not $ el `Set.member` prev) $ do
            lift $ modify (Set.insert el)
            yield el
        unique'

type EarliestDate = ByteString
type LatestDate = ByteString
type ShowName = ByteString
type DateRanges = Map ShowName (EarliestDate, LatestDate)

dateRange :: Monad m => Producer (Vector ByteString) m () -> m DateRanges
dateRange p = PP.fold accumulate initial id p where
    initial = Map.empty
    accumulate ranges entry = updatedRanges where
        series = entry ! 0
        date = entry ! 1
        existingRange = Map.lookup series ranges
        updatedRanges = case existingRange of
            Nothing -> Map.insert series (date, date) ranges
            Just (earliest, latest) -> Map.insert series (min earliest date, max latest date) ranges

traces :: Show a => Pipe a a IO ()
traces = do
    v <- await
    lift $ print v
    yield v

allBroadcasts = bstdin >-> records >-> selectField date >-> unique >-> prints

parseAndClean = records >-> PP.filter (\x -> V.length x == 11)

seriesNameAndDate = parseAndClean >-> selectFields [series, date]

main = do
    dates <- dateRange (bstdin >-> seriesNameAndDate)
    for_ (Map.toList dates) $ \(k, v) -> BS8.putStrLn $ mconcat [k, ", ", fst v, ", ", snd v]

csvFields@[childChannel, date, duration, epNo, parentChannel, progEpisode, series, seriesNo, source, startTime, version] = [0 .. 10]
