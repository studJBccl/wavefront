-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Codec.Wavefront.Line where

import Control.Lens
  
-- |A line index is a pair of indices. @'LineIndex' vi vti@. @vi@ references the locations and @vti@
-- indexes the texture coordinates. If @vti == 'Nothing'@, then that 'LineIndex' doesn’t have
-- texture coordinates associated with.
data LineIndex = LineIndex {
    _lineLocIndex :: {-# UNPACK #-} !Int
  , _lineTexCoordIndex :: !(Maybe Int)
  } deriving (Eq,Show)

$(makeLenses ''LineIndex)

-- A line gathers two line indices accessible by pattern matching or 'lineIndexA' and 'lineIndexB'.
data Line = Line {
    _lineIndexA :: LineIndex
  , _lineIndexB :: LineIndex
  } deriving (Eq,Show)

$(makeLenses ''Line)
