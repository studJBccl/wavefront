{-# LANGUAGE FlexibleInstances #-}

module Codec.Wavefront.UnToken (ObjText (..)) where

import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Object
import Codec.Wavefront.TexCoord
import Data.Bifoldable
import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Prelude hiding (lines)

class ObjText a where
  toObjText :: a -> T.Text

instance ObjText Location where
  toObjText (Location x y z w) = "v  " <> doubleSep [x, y, z, w]

instance ObjText TexCoord where
  toObjText (TexCoord r s t) = "vt  " <> doubleSep [r, s, t]

instance ObjText Normal where
  toObjText (Normal x y z) = "vn  " <> doubleSep [x, y, z]

instance ObjText FaceIndex where
  toObjText (FaceIndex locI mTexCoordI mNorI) = T.intercalate "/" $ map (maybe "" (T.pack . show)) [Just locI, mTexCoordI, mNorI]

instance ObjText Face where
  toObjText (Face i ii iii is) = "f  " <> doubleSep (i : ii : iii : is)

instance ObjText Float where
  toObjText = T.pack . show

-- Caution: overlapping
instance (Foldable f, ObjText a) => ObjText (f a) where
  toObjText = T.intercalate nl . map toObjText . toList

instance ObjText WavefrontOBJ where
  toObjText (WavefrontOBJ locs texCoords normals points lines faces mtlLibs) =
    let fsGroup = V.groupBy (\e1 e2 -> elObject e1 == elObject e2) faces
        fsMap = M.fromList $ fmap (\fs -> (V.uncons fs >>= Just . fst >>= elObject, fs)) fsGroup
        fsMap' = toObjText . fmap elValue <$> fsMap
        groupedFaces =
          bifoldr'
            (\mObjName s -> maybe nl (\n -> nl <> "o " <> n <> nl <> nl <> s) mObjName)
            (\objS s -> objS <> nl <> s)
            ""
            fsMap'
     in toObjText locs
          <> nl
          <> toObjText texCoords
          <> nl
          <> toObjText normals
          <> nl
          <> groupedFaces
          <> if not $ null mtlLibs then nl <> "mtllib " <> T.unwords (V.toList mtlLibs) else ""

-- TODO: Other elements props: groups...
-- TODO: Element Point
-- TODO: Element Line

doubleSep :: (Foldable f, ObjText a) => f a -> T.Text
doubleSep = T.intercalate "  " . map toObjText . toList

nl :: T.Text
nl = "\n"
