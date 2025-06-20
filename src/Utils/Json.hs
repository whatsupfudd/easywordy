module Utils.Json where

import qualified Data.Aeson as Ae

cutFieldP1 :: Ae.Options
cutFieldP1 = Ae.defaultOptions {
    Ae.fieldLabelModifier = init
  }

cutFieldP2 :: Ae.Options
cutFieldP2 = Ae.defaultOptions {
    Ae.fieldLabelModifier = init . init
  }

