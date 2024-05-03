{-# LANGUAGE PatternSynonyms #-}

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

module Codec.Wavefront.Face where

import Control.Lens

-- |A face index is a triplet of indices. @'FaceIndex' vi vti vni@ is a face that indexes the
-- locations with @vi@, the texture coordinates with @vti@ and the normals with @vni@. An index set
-- to 'Nothing' means /no information/. That is, if @vni == 'Nothing'@, then that 'FaceIndex'
-- doesn’t have a normal associated with.
data FaceIndex = FaceIndex {
    _faceLocIndex :: {-# UNPACK #-} !Int
  , _faceTexCoordIndex :: !(Maybe Int)
  , _faceNorIndex :: !(Maybe Int)
  } deriving (Eq,Show)

$(makeLenses ''FaceIndex)

-- |A face gathers several 'FaceIndex' to build up faces. It has a least three vertices
data Face = Face {
    _i :: FaceIndex
  , _ii :: FaceIndex
  , _iii :: FaceIndex
  , _is :: [FaceIndex]
  } deriving (Eq,Show)
  
$(makeLenses ''Face)

pattern Triangle :: FaceIndex -> FaceIndex -> FaceIndex -> Face
pattern Triangle a b c = Face a b c []

pattern Quad :: FaceIndex -> FaceIndex -> FaceIndex -> FaceIndex -> Face
pattern Quad a b c d = Face a b c [d]

faceIndices :: Face -> [FaceIndex]
faceIndices (Face _i _ii _iii _is) = _i:_ii:_iii:_is