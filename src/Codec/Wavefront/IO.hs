-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
module Codec.Wavefront.IO (fromFile, toFile) where

import Codec.Wavefront.Lexer (lexer)
import Codec.Wavefront.Object
import Codec.Wavefront.Token (tokenize)
import Codec.Wavefront.UnToken
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text.IO as TIO

-- | Extract a 'WavefrontOBJ' from a Wavefront OBJ formatted file.
fromFile :: (MonadIO m) => FilePath -> m (Either String WavefrontOBJ)
fromFile fd = liftIO $ fmap (fmap (ctxtToWavefrontOBJ . lexer) . tokenize) (TIO.readFile fd)

toFile :: FilePath -> WavefrontOBJ -> IO ()
toFile fp = TIO.writeFile fp . toObjText
