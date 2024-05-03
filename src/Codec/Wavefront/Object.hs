-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
module Codec.Wavefront.Object where

import Control.Lens
import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.Lexer (Ctxt (..))
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.TexCoord
import Data.DList (DList, toList)
import Data.Text (Text)
import Data.Vector (Vector, fromList)

data WavefrontOBJ = WavefrontOBJ
  { -- | Locations.
    _objLocations :: Vector Location,
    -- | Texture coordinates.
    _objTexCoords :: Vector TexCoord,
    -- | Normals.
    _objNormals :: Vector Normal,
    -- | Points.
    _objPoints :: Vector (Element Point),
    -- | Lines.
    _objLines :: Vector (Element Line),
    -- | Faces.
    _objFaces :: Vector (Element Face),
    -- | Material libraries.
    _objMtlLibs :: Vector Text
  }
  deriving (Eq, Show)

ctxtToWavefrontOBJ :: Ctxt -> WavefrontOBJ
ctxtToWavefrontOBJ ctxt =
  WavefrontOBJ
    { _objLocations = fromDList (ctxtLocations ctxt),
      _objTexCoords = fromDList (ctxtTexCoords ctxt),
      _objNormals = fromDList (ctxtNormals ctxt),
      _objPoints = fromDList (ctxtPoints ctxt),
      _objLines = fromDList (ctxtLines ctxt),
      _objFaces = fromDList (ctxtFaces ctxt),
      _objMtlLibs = fromDList (ctxtMtlLibs ctxt)
    }

fromDList :: DList a -> Vector a
fromDList = fromList . toList

$(makeLenses ''WavefrontOBJ)