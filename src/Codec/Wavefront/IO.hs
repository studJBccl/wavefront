-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
module Codec.Wavefront.IO (fromFile, toFile, allToFile) where

import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.Lexer (lexer)
import Codec.Wavefront.Line
import Codec.Wavefront.Object
import Codec.Wavefront.Point
import Codec.Wavefront.Token (tokenize)
import Codec.Wavefront.UnToken
import Control.Lens
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Extract a 'WavefrontOBJ' from a Wavefront OBJ formatted file.
fromFile :: (MonadIO m) => FilePath -> m (Either String WavefrontOBJ)
fromFile fd = liftIO $ fmap (fmap (ctxtToWavefrontOBJ . lexer) . tokenize) (TIO.readFile fd)

toFile :: FilePath -> WavefrontOBJ -> IO ()
toFile fp = TIO.writeFile fp . toObjText

allToFile :: FilePath -> [WavefrontOBJ] -> IO ()
allToFile fp =
  TIO.writeFile fp
    . T.intercalate "\n"
    . fmap (toObjText . uncurry offsets)
    . uncurry zip
    . first (scanl (+) 0)
    . unzip
    . fmap (\o -> (length (_objLocations o), o))
  where
    offsets :: Int -> WavefrontOBJ -> WavefrontOBJ
    offsets off o =
      o & objFaces . traverse . elValue . i . faceLocIndex %~ (+ off)
        & objFaces . traverse . elValue . ii . faceLocIndex %~ (+ off)
        & objFaces . traverse . elValue . iii . faceLocIndex %~ (+ off)
        & objFaces . traverse . elValue . is . traverse . faceLocIndex %~ (+ off)
        & objLines . traverse . elValue . lineIndexA . lineLocIndex %~ (+ off)
        & objLines . traverse . elValue . lineIndexB . lineLocIndex %~ (+ off)
        & objPoints . traverse . elValue . pointLocIndex %~ (+ off)
