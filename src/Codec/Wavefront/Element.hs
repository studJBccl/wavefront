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

module Codec.Wavefront.Element where

import Control.Lens
import Data.Text ( Text )
import Numeric.Natural ( Natural )

-- |An element holds a value along with the user-defined object’s name (if any), the associated
-- groups, the used material and the smoothing group the element belongs to (if any). Those values
-- can be used to sort the data per object or per group and to lookup materials.
data Element a = Element {
    _elObject :: Maybe Text
  , _elGroups :: [Text]
  , _elMtl :: Maybe Text
  , _elSmoothingGroup :: Natural
  , _elValue :: a
  } deriving (Eq,Show)

$(makeLenses ''Element)