module Main where

import Control.Monad
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Foldable
import Data.Traversable
import Data.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Pipes
import System.Environment
import System.IO

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
    lift $ BS.putStrLn bs
    bsprint

prints :: Show a => Consumer a IO ()
prints = do
    a <- await
    lift $ print a
    prints

passthrough = bstdin >-> bsprint

csv :: Monad m => Pipe ByteString (Vector ByteString) m ()
csv = do
    bs <- await
    let parsed = Csv.decode Csv.NoHeader (LBS.fromStrict bs)
    case parsed of
        Left err -> csv
        Right rs -> if V.null rs
            then csv
            else yield (V.head rs) >> csv

numFields :: Monad m => Pipe (Vector ByteString) Int m ()
numFields = do
    bs <- await
    yield (V.length bs)
    numFields

selectFields :: Monad m => [Int] -> Pipe (Vector a) (Vector a) m ()
selectFields fs = do
    iv <- await
    let ov = create $ do
            v <- MV.new (Prelude.length fs)
            for_ (Prelude.zip [0..] fs) (\(i, ix) -> MV.write v i (iv ! ix))
            return v
    yield ov
    selectFields fs

csvLength = bstdin >-> csv >-> selectFields [1, 2] >-> numFields >-> prints

main = getArgs >>= runEffect . dispatch

dispatch ["passthrough"] = passthrough
dispatch _ = csvLength
